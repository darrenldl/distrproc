module Proc : sig
  module Pid : sig
    type t

    val pp : Format.formatter -> t -> unit
  end

  module Handle : sig
    type t

    val pid : t -> Pid.t

    val env : t -> Eio.Stdenv.t

    val sw : t -> Eio.Switch.t
  end
end

module Mailbox : sig
  module Local : sig
    type 'a t

    type 'a interface = {
      send : Proc.Handle.t -> (Proc.Pid.t * 'a) -> unit;
      recv : Proc.Handle.t -> Proc.Pid.t * 'a;
    }

    val make : unit -> 'a t

    val interface : 'a t -> 'a interface
  end
end

module Selective_recv : sig
  type 'a guard = Proc.Pid.t * 'a -> bool

  type ('a, 'b) body = Proc.Pid.t * 'a -> 'b

  type ('a, 'b) entry

  val entry : ?guard:'a guard -> ('a, 'b) body -> ('a, 'b) entry

  type 'a case

  val case_local : 'a Mailbox.Local.t -> ('a, 'b) entry list -> 'b case

  val f : Proc.Handle.t -> ?timeout:(float * (unit -> 'b)) -> 'b case list -> 'b
end

module Gateway : sig
  type t

  val spawn : (Proc.Handle.t -> unit) -> Proc.Pid.t

  val main : Eio.Stdenv.t -> unit
end
