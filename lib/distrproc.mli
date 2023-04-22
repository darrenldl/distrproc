module Proc : sig
  module Pid : sig
    type t

    type local

    val local : t -> local

    val pp : Format.formatter -> t -> unit
  end

  module Handle : sig
    type t

    val pid : t -> Pid.t
  end
end

module Mailbox : sig
  module Local : sig
  type 'a t = {
    send : Proc.Handle.t -> Proc.Pid.t -> 'a -> unit;
    recv : Proc.Handle.t -> Proc.Pid.t * 'a;
  }
val make : unit -> 'a t
  end
end

module Gw : sig
  type t

  val make : unit -> t

  val run : t -> (Proc.Handle.t -> unit) -> Proc.Pid.t

  val join : t -> unit
end
