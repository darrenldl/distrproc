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

    let make ~gw_id : t =
      let local = !counter in
      counter := !counter + 1;
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

module Mailbox = struct
  module Local = struct
    type 'a t = {
      send : Proc.Handle.t -> Proc.Pid.local -> 'a -> unit;
      recv : Proc.Handle.t -> Proc.Pid.t * 'a;
    }
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
    let pid = Proc.Pid.make ~gw_id:t.id in
    let h = Proc.Handle.make ~pid in
    let promise = D.Task.async t.pool (fun () -> p h) in
    Mutex.lock t.lock;
    t.promises <- promise :: t.promises;
    Mutex.unlock t.lock;
    pid

  let join (t : t) =
    List.fold_left (fun () promise ->
        D.Task.await t.pool promise
      )
      ()
      t.promises

end

let register_local_mailbox (type a) () : a Mailbox.Local.t =
  let table : (Proc.Pid.local, (Proc.Pid.t * a) K_d.Queue.t) K_d.Hashtbl.t =
    K_d.Hashtbl.create ()
  in
  let send (h : Proc.Handle.t) (pid : Proc.Pid.local) (msg : a) : unit =
    match K_d.Hashtbl.find_opt table pid with
    | None -> K_d.Hashtbl.replace table pid (K_d.Queue.create ());
    | Some q -> K_d.Queue.add (Proc.Handle.pid h, msg) q
  in
  let recv (h : Proc.Handle.t) : Proc.Pid.t * a =
    let pid = Proc.Handle.pid h in
    let local_pid = Proc.Pid.local pid in
    let gw = Gw.lookup_gw ~gw_id:(Proc.Pid.gw_id pid) in
    let rec aux () =
      match K_d.Hashtbl.find_opt table local_pid with
      | None -> K_d.Hashtbl.replace table local_pid (K_d.Queue.create ()); aux ()
      | Some q ->
        match K_d.Queue.take_opt q with
        | None -> yield gw.pool; aux ()
        | Some (src, x) -> (src, x)
    in
    aux ()
  in
  {
    send;
    recv;
  }
