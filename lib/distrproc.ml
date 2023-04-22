module D = Domainslib
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
    }

    let make ~pid = { pid }

    let pid (t : t) = t.pid
  end
end

let yield pool = D.Task.(await pool (async pool (fun () -> ())))

module Gw = struct
  type t = {
    id : int;
    pool : D.Task.pool;
    lock : Mutex.t;
    mutable promises : unit D.Task.promise list;
  }

  type collection = {
    lock : Mutex.t;
    mutable counter : int;
    table : (gw_id, t) K_d.Hashtbl.t;
  }

  let collection : collection = {
    lock = Mutex.create ();
    counter = 0;
    table = K_d.Hashtbl.create ();
  }

  let lookup_gw ~gw_id =
    K_d.Hashtbl.find collection.table gw_id

  let make () =
    let id = collection.counter in
    Mutex.lock collection.lock;
    collection.counter <- id + 1;
    Mutex.unlock collection.lock;
    let pool = D.Task.setup_pool ~num_domains:4 () in (* TODO: detect core count *)
    let t = {
      id;
      lock = Mutex.create ();
      pool;
      promises = [];
    }
    in
    K_d.Hashtbl.replace collection.table id t;
    t

  let run (t : t) (p : Proc.Handle.t -> unit) : Proc.Pid.t =
    let pid = Proc.Pid.make_fresh ~gw_id:t.id in
    let h = Proc.Handle.make ~pid in
    let promise = D.Task.async t.pool (fun () -> p h) in
    Mutex.lock t.lock;
    t.promises <- promise :: t.promises;
    Mutex.unlock t.lock;
    pid

  let join (t : t) =
    D.Task.run t.pool (fun () ->
        List.fold_left (fun () promise ->
            D.Task.await t.pool promise;
          )
          ()
          t.promises
      )
end

module Mailbox = struct
  module Local = struct
    type 'a t = {
      send : Proc.Handle.t -> Proc.Pid.t -> 'a -> unit;
      recv : Proc.Handle.t -> Proc.Pid.t * 'a;
    }

    type 'a mailbox = {
      lock : Mutex.t;
      table : (Proc.Pid.local, (Proc.Pid.local * 'a) K_d.Queue.t) Hashtbl.t;
    }

    let make (type a) () : a t =
      let mailbox : a mailbox =
        {
          lock = Mutex.create ();
          table = Hashtbl.create 100;
        }
      in
      let send (h : Proc.Handle.t) (pid : Proc.Pid.t) (msg : a) : unit =
        let self_pid = Proc.Handle.pid h in
        if Proc.(Pid.same_gw self_pid pid) then (
          let local_pid = Proc.Pid.local pid in
          let q =
            Mutex.lock mailbox.lock;
            let q =
              match Hashtbl.find_opt mailbox.table local_pid with
              | None -> let q = K_d.Queue.create () in
                Hashtbl.replace mailbox.table local_pid q;
                q
              | Some q -> q
            in
            Mutex.unlock mailbox.lock;
            q
          in
          K_d.Queue.add (Proc.Pid.local self_pid, msg) q
        )
      in
      let recv (h : Proc.Handle.t) : Proc.Pid.t * a =
        let self_pid = Proc.Handle.pid h in
        let self_local_pid = Proc.Pid.local self_pid in
        let gw_id = Proc.Pid.gw_id self_pid in
        let gw = Gw.lookup_gw ~gw_id in
        let rec aux () =
          let q =
            Mutex.lock mailbox.lock;
            let q = Hashtbl.find_opt mailbox.table self_local_pid in
            Mutex.unlock mailbox.lock;
            q
          in
          match q with
          | None -> (
              yield gw.pool;
              aux ()
            )
          | Some q -> (
              match K_d.Queue.take_opt q with
              | None -> yield gw.pool; aux ()
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

