let rec reverseList list =
    match list with
    | [] -> list
    | h :: t -> List.append (reverseList list.Tail) [list.Head]

printfn "List: [1; 2; 3; 4; 5]"

printfn "Result: %A" (reverseList [1; 2; 3; 4; 5])