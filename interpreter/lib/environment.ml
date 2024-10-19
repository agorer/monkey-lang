module StringMap = Map.Make(String)
    
type 'a t = {
  store: 'a StringMap.t
}

let make () =
  { store = StringMap.empty }

let get env name =
  StringMap.find_opt name env.store

let set env name value =
  { store = StringMap.add name value env.store }

(* let pp formatter _env = *)
(*   Format.fprintf formatter "<env not printable" *)
