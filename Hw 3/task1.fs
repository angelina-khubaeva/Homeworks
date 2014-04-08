let firstPosition (number: int) list =
    let rec assistant (list1: List<int>) =
        if list1.Length = 1 then list1
            elif list1.Head = number then [number]
            else List.append (assistant list1.Tail) (assistant [list1.Head])
    let listAssist = assistant list
    let position = listAssist.Length - 1
    position

let positionMaxSum list =
    let list0 = list @ [0]
    let listSum = List.map2 (fun x y -> x + y) list list0.Tail
    let max = List.fold (fun acc x -> if acc < x then x else acc) 0 listSum
    let position = firstPosition max listSum
    position

printfn "%A" (positionMaxSum [1; 5; 6; 2])   // 1
printfn "%A" (positionMaxSum [0; 0; 0; 2])   // 2
printfn "%A" (positionMaxSum [1; 0; 0; 0])   // 0
printfn "%A" (positionMaxSum [1; 5; 6; 77])  // 2
printfn "%A" (positionMaxSum [1; 5; 6; 2; 8; 99; 365; 101])  // 6
