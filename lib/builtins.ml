open Object

module Builtins = struct
  type builtinFnTable = (string, data_obj) Hashtbl.t

  let len_implementation (args : data_obj list) : data_obj =
    match args with
    | [ StringObj arg ] -> IntegerObj (String.length arg)
    | [ x ] ->
        new_error "argument to `len` not supported, got %s"
          (type_string_of_object x)
    | _ ->
        new_error "wrong number of arguments. got=%d, want=%d"
          (List.length args) 1

  let table : builtinFnTable =
    let b = Hashtbl.create 10 in
    Hashtbl.add b "len" (BuiltinFnObj len_implementation);
    b

  let find_opt (key : string) : data_obj option = Hashtbl.find_opt table key
end
