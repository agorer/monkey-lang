module StringMap = Map.Make(String)
    
type t = {
  store: Object.t StringMap.t
}

let make () =
  { store = StringMap.empty }

let get env name =
  StringMap.find_opt name env.store

let set env name value =
  { store = StringMap.add name value env.store }
