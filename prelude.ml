
let bracket res destroy k = Std.finally (fun () -> destroy res) k res
let (>>) x f = f x
let catch f x = try Some (f x) with _ -> None
external id : 'a -> 'a = "%identity"
let some x = Some x

