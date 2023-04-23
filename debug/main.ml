open Distrproc

type x =
  | A
  | B

let pp_x formatter (x : x) =
  Fmt.pf formatter "%s" (match x with
      | A -> "A"
      | B -> "B")

let () =
  let { send = send_pid; recv = recv_pid } : Proc.Pid.t Mailbox.Local.t =
    Mailbox.Local.make ()
  in
  let { send = send_x; recv = recv_x } : x Mailbox.Local.t =
    Mailbox.Local.make ()
  in
  let a =
    Gateway.attach (fun h ->
        Fmt.epr "a: my pid is %a@." Proc.Pid.pp (Proc.Handle.pid h);
        let _, send_to = recv_pid h in
        Fmt.epr "a: received instruction to send to %a@." Proc.Pid.pp send_to;
        send_x h send_to A;
        let from, msg = recv_x h in
        Fmt.epr "a: received %a from %a@." pp_x msg Proc.Pid.pp from;
      )
  in
  let b =
    Gateway.attach (fun h ->
        Fmt.epr "b: my pid is %a@." Proc.Pid.pp (Proc.Handle.pid h);
        let _, send_to = recv_pid h in
        Fmt.epr "b: received instruction to send to %a@." Proc.Pid.pp send_to;
        send_x h send_to B;
        let from, msg = recv_x h in
        Fmt.epr "b: received %a from %a@,@." pp_x msg Proc.Pid.pp from;
      )
  in
  let _controller =
    Gateway.attach (fun h ->
        Fmt.epr "controller: my pid is %a@." Proc.Pid.pp (Proc.Handle.pid h);
        send_pid h a b;
        Fmt.epr "controller: sent instructions to a@.";
        send_pid h b a;
        Fmt.epr "controller: sent instructions to b@.";
      )
  in
  Eio_main.run Gateway.main

(*
Example output:
  a: my pid is (0, 0)
  b: my pid is (0, 1)
  controller: my pid is (0, 2)
  controller: sent instructions to a
  a: received instruction to send to (0, 1)
  controller: sent instructions to b
  b: received instruction to send to (0, 0)
  a: received B from (0, 1)
  b: received A from (0, 0)
*)
