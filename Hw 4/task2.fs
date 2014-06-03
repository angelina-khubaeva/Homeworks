// Записать в point-free стиле 
// func x l = List.map (fun y -> y * x) l

let func = List.map << (*)
