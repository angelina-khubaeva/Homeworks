// Функция, которая выдает первую позицию вхождения заданного числа в список

let firstPosition (number: int) (list: List<int>) =
    let rec assistant (list1: List<int>) =
        if list1.Length = 1 then list1
            elif list1.Head = number then [number]
            else List.append (assistant list1.Tail) [list1.Head]
    let listAssist = assistant list
    if List.exists (fun elem -> elem = number) listAssist
    then listAssist.Length
    else -1

printfn "List: [3; 4; 1; 8; 45; 7]; number: 7"
printfn "Result: %A" <| firstPosition 7 [3; 4; 1; 8; 45; 7]

printfn "\nList: [7; 3; 4; 1; 8; 45]; number: 7"
printfn "Result: %A" <| firstPosition 7 [7; 3; 4; 1; 8; 45]

printfn "\nList: [7; 3; 4; 1; 8; 45]; number: 1"
printfn "Result: %A" <| firstPosition 1 [7; 3; 4; 1; 8; 45]

printfn "\nList: [3; 4; 1; 8; 45; 7]; number: 777"
printfn "Result: %A" <| firstPosition 777 [3; 4; 1; 8; 45; 7]
