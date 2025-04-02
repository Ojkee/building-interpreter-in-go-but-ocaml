open Object

module Builtins : sig
  type builtinFnTable = (string, data_obj) Hashtbl.t

  val find_opt : string -> data_obj option
end
