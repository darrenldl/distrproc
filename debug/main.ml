open Distrproc

type x =
  | A
  | B
  | C

let pp_x formatter (x : x) =
  Fmt.pf formatter "%s" (match x with
      | A -> "A"
      | B -> "B"
      | C -> "C")

let () =
  let pid_mailbox : Proc.Pid.t Mailbox.Local.t =
    Mailbox.Local.make ()
  in
  let Mailbox.Local.{ send = send_pid; recv = recv_pid } = Mailbox.Local.interface pid_mailbox in
  let x_mailbox  : x Mailbox.Local.t =
    Mailbox.Local.make ()
  in
  let Mailbox.Local.{ send = send_x; recv = recv_x } = Mailbox.Local.interface x_mailbox in
  let a =
    Gateway.spawn (fun h ->
        Fmt.epr "a: my pid is %a@." Proc.Pid.pp (Proc.Handle.pid h);

        let _, send_to = recv_pid h in
        Fmt.epr "a: received instruction to send to %a@." Proc.Pid.pp send_to;
        send_x h (send_to, A);
        send_x h (send_to, A);
        send_x h (send_to, A);

        let rec aux () =
          let success =
            Selective.recv h
              ~timeout:(1.0, fun () ->
                  Fmt.epr "a: I haven't received anything useful yet@.";
                  false
                )
              Selective.Recv.[
                case_local x_mailbox
                  [
                    entry ~guard:(fun (from, x) -> x = A)
                      (fun (from, msg) ->
                         Fmt.epr "a: received %a from %a@." pp_x msg Proc.Pid.pp from;
                         true
                      );
                    entry ~guard:(fun (from, x) -> x = B)
                      (fun (from, msg) ->
                         Fmt.epr "a: received %a from %a@." pp_x msg Proc.Pid.pp from;
                         true
                      );
                  ]
              ]
          in
          if not success then
            aux ()
        in
        aux ()
      )
  in
  let b =
    Gateway.spawn (fun h ->
        Fmt.epr "b: my pid is %a@." Proc.Pid.pp (Proc.Handle.pid h);
        let _, send_to = recv_pid h in
        Fmt.epr "b: received instruction to send to %a@." Proc.Pid.pp send_to;

        let clock = Eio.Stdenv.clock (Proc.Handle.env h) in

        send_x h (send_to, C);
        send_x h (send_to, C);
        send_x h (send_to, C);
        send_x h (send_to, C);
        send_x h (send_to, C);
        send_x h (send_to, C);

        Eio.Time.sleep clock 5.0;

        send_x h (send_to, A);

        let from, msg = recv_x h in
        Fmt.epr "b: received %a from %a@." pp_x msg Proc.Pid.pp from;
      )
  in
  let _controller =
    Gateway.spawn (fun h ->
        Fmt.epr "controller: my pid is %a@." Proc.Pid.pp (Proc.Handle.pid h);
        send_pid h (a, b);
        Fmt.epr "controller: sent instructions to a@.";
        send_pid h (b, a);
        Fmt.epr "controller: sent instructions to b@.";
      )
  in
  Eio_main.run Gateway.main

(*
Example output:
  a: my pid is (0, 3)
  b: my pid is (0, 4)
  controller: my pid is (0, 5)
  a: received instruction to send to (0, 4)
  controller: sent instructions to a
  b: received instruction to send to (0, 3)
  controller: sent instructions to b
  a: I haven't received anything useful yet
  a: I haven't received anything useful yet
  a: I haven't received anything useful yet
  a: I haven't received anything useful yet
  b: received A from (0, 3)
  a: I haven't received anything useful yet
  a: received A from (0, 4)
*)
