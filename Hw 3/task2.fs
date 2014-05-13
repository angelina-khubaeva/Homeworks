// #3.2
// Проверить, что все элементы списка различны 

let isAllDifferent (list: List<'a>) = 
    let containsElem element list = List.exists (fun elem -> elem = element) list
    let rec assist acc (list1: List<'a>) = 
        if acc = true then "Not all elements are different"
        elif list1.Length = 1 then "All elements are different"
        else assist (containsElem list.Head list.Tail) list1.Tail
    assist (containsElem list.Head list.Tail) list

printfn "%A" <| isAllDifferent [1; 2; 3; 4; 5]
printfn "%A" <| isAllDifferent [1; 2; 3; 1; 5]
