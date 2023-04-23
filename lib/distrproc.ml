open Eio.Std
module K_d = Kcas_data

type gw_id = int

module Proc = struct
  module Pid = struct
    let counter = ref 0

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
      let local = !counter in
      counter := !counter + 1;
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
    let id = Random.int 1000 in (* TODO: replace with better id generation *)
    {
      id;
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

  let attach (p : Proc.Handle.t -> unit) : Proc.Pid.t =
    let pid = Proc.Pid.make_fresh ~gw_id:t.id in
    Eio.Stream.add t.proc_queue (pid, p);
    pid

  (* let attach_to_new_domain (t : t) (p : Proc.Handle.t -> unit) : Proc.Pid.t = *)
end

module Mailbox = struct
  module Local = struct
    type 'a t = {
      send : Proc.Handle.t -> Proc.Pid.t -> 'a -> unit;
      recv : Proc.Handle.t -> Proc.Pid.t * 'a;
    }

    type 'a mailbox = {
      lock : Eio.Mutex.t;
      table : (Proc.Pid.local, (Proc.Pid.local * 'a) K_d.Queue.t) Hashtbl.t;
    }

    let make (type a) () : a t =
      let mailbox : a mailbox =
        {
          lock = Eio.Mutex.create ();
          table = Hashtbl.create 100;
        }
      in
      let send (h : Proc.Handle.t) (pid : Proc.Pid.t) (msg : a) : unit =
        let self_pid = Proc.Handle.pid h in
        if Proc.(Pid.same_gw self_pid pid) then (
          let local_pid = Proc.Pid.local pid in
          let q =
            Eio.Mutex.lock mailbox.lock;
            let q =
              match Hashtbl.find_opt mailbox.table local_pid with
              | None -> let q = K_d.Queue.create () in
                Hashtbl.replace mailbox.table local_pid q;
                q
              | Some q -> q
            in
            Eio.Mutex.unlock mailbox.lock;
            q
          in
          K_d.Queue.add (Proc.Pid.local self_pid, msg) q
        )
      in
      let recv (h : Proc.Handle.t) : Proc.Pid.t * a =
        let self_pid = Proc.Handle.pid h in
        let self_local_pid = Proc.Pid.local self_pid in
        let gw_id = Proc.Pid.gw_id self_pid in
        let rec aux () =
          let q =
            Eio.Mutex.lock mailbox.lock;
            let q = Hashtbl.find_opt mailbox.table self_local_pid in
            Eio.Mutex.unlock mailbox.lock;
            q
          in
          match q with
          | None -> (
              Eio.Fiber.yield ();
              aux ()
            )
          | Some q -> (
              match K_d.Queue.take_opt q with
              | None -> Eio.Fiber.yield (); aux ()
              | Some (src, x) -> (Proc.Pid.make ~gw_id src, x)
            )
        in
        aux ()
      in
      {
        send;
        recv;
      }
  end
end

