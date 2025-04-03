open Object

module Builtins = struct
  type builtinFnTable = (string, data_obj) Hashtbl.t

  let len_impl (args : data_obj list) : data_obj =
    match args with
    | [ StringObj arg ] -> IntegerObj (String.length arg)
    | [ ArrayObj elements ] -> IntegerObj (List.length elements)
    | [ x ] ->
        new_error "argument to `len` not supported, got: %s"
          (type_string_of_object x)
    | _ ->
        new_error "wrong number of arguments. (want: 1, got: %d)"
          (List.length args)

  let first_impl (args : data_obj list) : data_obj =
    match args with
    | [ ArrayObj [] ] -> NullObj
    | [ ArrayObj (h :: _) ] -> h
    | [ x ] ->
        new_error "argument to `first` must be ARRAY, got: %s"
          (type_string_of_object x)
    | _ ->
        new_error "wrong number of arguments. (want: 1, got: %d)"
          (List.length args)

  let last_impl (args : data_obj list) : data_obj =
    let rec last_elem elements =
      match elements with [] -> NullObj | [ x ] -> x | _ :: t -> last_elem t
    in
    match args with
    | [ ArrayObj [] ] -> NullObj
    | [ ArrayObj (_ :: t) ] -> last_elem t
    | [ x ] ->
        new_error "argument to `last` must be ARRAY, got: %s"
          (type_string_of_object x)
    | _ ->
        new_error "wrong number of arguments. (want: 1, got: %d)"
          (List.length args)

  let rest_impl (args : data_obj list) : data_obj =
    match args with
    | [ ArrayObj [] ] -> NullObj
    | [ ArrayObj (_ :: t) ] -> ArrayObj t
    | [ x ] ->
        new_error "argument to `rest` must be ARRAY, got: %s"
          (type_string_of_object x)
    | _ ->
        new_error "wrong number of arguments. (want: 1, got: %d)"
          (List.length args)

  let push_impl (args : data_obj list) : data_obj =
    match args with
    | [ ArrayObj elements; x ] -> ArrayObj (elements @ [ x ])
    | [ x; _ ] ->
        new_error "argument to `last` must be ARRAY, got: %s"
          (type_string_of_object x)
    | _ ->
        new_error "wrong number of arguments. (want: 2, got: %d)"
          (List.length args)

  let table : builtinFnTable =
    let fn_implementations = Hashtbl.create 10 in
    Hashtbl.add fn_implementations "len" (BuiltinFnObj len_impl);
    Hashtbl.add fn_implementations "first" (BuiltinFnObj first_impl);
    Hashtbl.add fn_implementations "last" (BuiltinFnObj last_impl);
    Hashtbl.add fn_implementations "rest" (BuiltinFnObj rest_impl);
    Hashtbl.add fn_implementations "push" (BuiltinFnObj push_impl);
    fn_implementations

  let find_opt (key : string) : data_obj option = Hashtbl.find_opt table key
end
