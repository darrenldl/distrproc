open Eio.Std
module K_d = Kcas_data

type gw_id = int

let counter = Atomic.make 0

let counter_fetch_and_incr () =
  Atomic.fetch_and_add counter 1

module Proc = struct
  module Pid = struct
    type local = int

    type t = {
      gw_id : gw_id;
      local : local;
    }

    let local (t : t) = t.local

    let gw_id (t : t) = t.gw_id

    let same_gw (x : t) (y : t) =
      x.gw_id = y.gw_id

    let make_fresh ~gw_id : t =
      let local = counter_fetch_and_incr () in
      { gw_id; local }

    let make ~gw_id local : t =
      { gw_id; local }

    let pp formatter (t : t) =
      Fmt.pf formatter "(%d, %d)" t.gw_id t.local
  end

  module Handle = struct
    type t = {
      pid : Pid.t;
      env : Eio.Stdenv.t;
      sw : Eio.Switch.t;
    }

    let make ~pid ~env ~sw = { pid; env; sw }

    let pid (t : t) = t.pid

    let env (t : t) = t.env

    let sw (t : t) = t.sw
  end
end

module Gateway = struct
  type proc = (Proc.Handle.t -> unit)

  type t = {
    id : int;
    lock : Eio.Mutex.t;
    proc_queue : (Proc.Pid.t * proc) Eio.Stream.t;
    mutable proc_promises : unit Eio.Promise.or_exn list;
  }

  let t =
    {
      id = counter_fetch_and_incr ();
      lock = Eio.Mutex.create ();
      proc_queue = Eio.Stream.create 1000;
      proc_promises = [];
    }

  let main (env : Eio.Stdenv.t) : unit =
    Switch.run (fun sw ->
        let rec aux () =
          match Eio.Stream.take_nonblocking t.proc_queue with
          | Some (pid, proc) -> (
              let promise =
                Eio.Fiber.fork_promise ~sw (fun () ->
                    Switch.run (fun sw ->
                        let handle = Proc.Handle.make ~pid ~env ~sw in
                        proc handle
                      )
                  )
              in
              Eio.Mutex.lock t.lock;
              t.proc_promises <- promise :: t.proc_promises;
              Eio.Mutex.unlock t.lock;
              aux ()
            )
          | None -> (
              let all_procs_finished =
                Eio.Mutex.lock t.lock;
                let r =
                  List.for_all (fun p -> Eio.Promise.is_resolved p) t.proc_promises
                in
                Eio.Mutex.unlock t.lock;
                r
              in
              if all_procs_finished then
                ()
              else (
                Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
                aux ()
              )
            )
        in
        aux ()
      )

  let spawn (p : Proc.Handle.t -> unit) : Proc.Pid.t =
    let pid = Proc.Pid.make_fresh ~gw_id:t.id in
    Eio.Stream.add t.proc_queue (pid, p);
    pid

  (* let spawn_to_new_domain (t : t) (p : Proc.Handle.t -> unit) : Proc.Pid.t = *)
end

module Mailbox = struct
  module Local = struct
    type 'a q = (Proc.Pid.local * 'a) K_d.Queue.t

    type 'a queues = {
      main : 'a q;
      save : 'a q; (* same purpose as Erlang's save queue, used as buffer for items which fail the guards *)
    }

    type 'a t = {
      id : int;
      lock : Eio.Mutex.t;
      queues : (Proc.Pid.local, 'a queues) Hashtbl.t;
    }

    type 'a interface = {
      send : Proc.Handle.t -> (Proc.Pid.t * 'a) -> unit;
      recv : Proc.Handle.t -> Proc.Pid.t * 'a;
    }

    let make (type a) () : a t =
      {
        id = counter_fetch_and_incr ();
        lock = Eio.Mutex.create ();
        queues = Hashtbl.create 100;
      }

    let add_queues table local_pid =
      let main = K_d.Queue.create () in
      let save = K_d.Queue.create () in
      Hashtbl.replace table local_pid { main; save };
      { main; save }

    let send (type a) (t : a t) (h : Proc.Handle.t) ((pid, msg) : Proc.Pid.t * a) : unit =
      let self_pid = Proc.Handle.pid h in
      if Proc.(Pid.same_gw self_pid pid) then (
        let local_pid = Proc.Pid.local pid in
        let q =
          Eio.Mutex.lock t.lock;
          let q =
            match Hashtbl.find_opt t.queues local_pid with
            | None ->
              let { main; save = _ } = add_queues t.queues local_pid in
              main
            | Some { main; save = _ } -> main
          in
          Eio.Mutex.unlock t.lock;
          q
        in
        K_d.Queue.add (Proc.Pid.local self_pid, msg) q;
        Eio.Fiber.yield ()
      )

    let requeue (type a) (t : a t) (h : Proc.Handle.t) ((pid, msg) : Proc.Pid.t * a) : unit =
      let self_local_pid = Proc.Pid.local (Proc.Handle.pid h) in
      let q =
        Eio.Mutex.lock t.lock;
        let q =
          match Hashtbl.find_opt t.queues self_local_pid with
          | None ->
            let { main = _; save } = add_queues t.queues self_local_pid in
            save
          | Some { main = _; save } -> save
        in
        Eio.Mutex.unlock t.lock;
        q
      in
      K_d.Queue.add (Proc.Pid.local pid, msg) q;
      Eio.Fiber.yield ()

    let empty_save_into_main ~save ~main =
      let rec aux () =
        match K_d.Queue.take_opt save with
        | None -> ()
        | Some x -> (
            K_d.Queue.add x main;
            aux ()
          )
      in
      aux ()

    let recv (type a) (t : a t) (h : Proc.Handle.t) : Proc.Pid.t * a =
      let self_pid = Proc.Handle.pid h in
      let self_local_pid = Proc.Pid.local self_pid in
      let gw_id = Proc.Pid.gw_id self_pid in
      let rec aux () =
        let q =
          Eio.Mutex.lock t.lock;
          let q = Hashtbl.find_opt t.queues self_local_pid in
          Eio.Mutex.unlock t.lock;
          q
        in
        match q with
        | None -> (
            Eio.Fiber.yield ();
            aux ()
          )
        | Some { main; save } -> (
            empty_save_into_main ~save ~main;
            match K_d.Queue.take_opt main with
            | None -> Eio.Fiber.yield (); aux ()
            | Some (src, x) -> (Proc.Pid.make ~gw_id src, x)
          )
      in
      aux ()

    let interface (type a) (t : a t) : a interface =
      {
        send = send t;
        recv = recv t;
      }
  end
end

module Selective = struct
  module Recv = struct
    type ctx = {
      received_msg : bool Atomic.t;
      timed_out : bool Atomic.t;
    }

    let make_ctx () = {
      received_msg = Atomic.make false;
      timed_out = Atomic.make false;
    }

    type 'a guard = Proc.Pid.t * 'a -> bool

    type ('a, 'b) body = Proc.Pid.t * 'a -> 'b

    type ('a, 'b) entry = {
      guard : 'a guard option;
      body : ('a, 'b) body;
    }

    let entry (type a b)
        ?(guard : a guard option)
        (body : (a, b) body)
      : (a, b) entry
      =
      { guard; body }

    type 'b case = Proc.Handle.t -> ctx -> unit -> 'b

    let case_local (type a b)
        (mailbox : a Mailbox.Local.t)
        (entries : (a, b) entry list)
      : b case =
      fun h ctx () ->
      let rec try_all (l : (a, b) entry list) (pid, msg) : (a, b) body option =
        match l with
        | [] -> None
        | { guard; body } :: rest -> (
            match guard with
            | None -> Some body
            | Some guard ->
              if guard (pid, msg) then (
                Some body
              ) else (
                try_all rest (pid, msg)
              )
          )
      in
      let rec aux () =
        let r = Mailbox.Local.recv mailbox h in
        match try_all entries r with
        | None -> Mailbox.Local.requeue mailbox h r; aux ()
        | Some body -> (
            Atomic.set ctx.received_msg true;
            body r
          )
      in
      aux ()

    let f (type b)
        (h : Proc.Handle.t)
        ?(timeout : (float * (unit -> b)) option)
        (cases : b case list)
      : b =
      let clock = Eio.Stdenv.clock (Proc.Handle.env h) in
      let ctx = make_ctx () in
      let l = (List.map (fun case -> case h ctx) cases)
      in
      let l =
        match timeout with
        | None -> l
        | Some (timeout, timeout_handler) ->
          (fun () ->
             Eio.Time.sleep clock timeout;
             Atomic.set ctx.timed_out true;
             if Atomic.get ctx.received_msg then
               Eio.Fiber.await_cancel ()
             else
               timeout_handler ()
          )
          :: l
      in
      Eio.Fiber.any l
  end

  let recv = Recv.f
end
