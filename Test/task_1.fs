// [1, -1, 1, -1, ...]
let seqInfinite1 = Seq.initInfinite (fun index ->
    if index % 2 = 0 then 1 else -1)

// [1, 2, 3, 4, ...]
let seqInfinite2 = Seq.initInfinite (fun index -> index + 1)

// [1, -2, 3, -4, ...] 
let seqInfinite = Seq.map2 (fun x y -> x * y) seqInfinite1 seqInfinite2

printfn "%A" seqInfinite