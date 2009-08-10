
let bracket res destroy k = Std.finally (fun () -> destroy res) k res
let (>>) x f = f x
 
