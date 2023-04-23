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

    val env : t -> Eio.Stdenv.t

    val sw : t -> Eio.Switch.t
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

module Gateway : sig
  type t

  val attach : (Proc.Handle.t -> unit) -> Proc.Pid.t

  val main : Eio.Stdenv.t -> unit
end
