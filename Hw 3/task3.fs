let numberOfEvenNumbersWithMapAndFilter list = 
    let newList01 = List.map (fun x -> (x % 2)) list 
    let zeroOnlyList = List.filter (fun x -> x = 0) newList01
    let number = zeroOnlyList.Length
    number

let numberOfEvenNumbersWithFilter list = 
    let evenOnlyList = List.filter (fun x -> x % 2 = 0) list
    let number = evenOnlyList.Length
    number

let numberOfEvenNumbersWithFilterAndFold list =
    let count = List.fold (fun acc x -> acc + (x % 2)) 0 list
    let length = list.Length
    let number = length - count
    number

printfn "List: [1; 2; 3; 4; 5; 6; 7]"
printfn "Result:"
printfn "%A" (numberOfEvenNumbersWithMapAndFilter [1; 2; 3; 4; 5; 6; 7])                // 3

printfn "\nList: [1; 2; 3; 4; 5; 6; 7; 0; 0; 0; 0]"
printfn "Result:"
printfn "%A" (numberOfEvenNumbersWithMapAndFilter [1; 2; 3; 4; 5; 6; 7; 0; 0; 0; 0])    // 7

printfn "\nList: [0; 2; 78; 4; 90; 6; 52; 7]"
printfn "Result:"
printfn "%A" (numberOfEvenNumbersWithMapAndFilter [0; 2; 78; 4; 90; 6; 52; 7])          // 7 

printfn "\nList: [1; 2; 3; 4; 5; 6; 7]"
printfn "Result:"
printfn "%A" (numberOfEvenNumbersWithFilter [1; 2; 3; 4; 5; 6; 7])                      // 3

printfn "\nList: [1; 2; 3; 4; 5; 6; 7; 0; 0; 0; 0;]"
printfn "Result:"
printfn "%A" (numberOfEvenNumbersWithFilter [1; 2; 3; 4; 5; 6; 7; 0; 0; 0; 0;])         // 7

printfn "\nList: [0; 2; 78; 4; 90; 6; 7; 52]"
printfn "Result:"
printfn "%A" (numberOfEvenNumbersWithFilter [0; 2; 78; 4; 90; 6; 7; 52])                // 7

printfn "\nList: [1; 2; 3; 4; 5; 6; 7]"
printfn "Result:"
printfn "%A" (numberOfEvenNumbersWithFilterAndFold [1; 2; 3; 4; 5; 6; 7])               // 3

printfn "\nList: [1; 2; 3; 4; 5; 6; 7; 0; 0; 0; 0]"
printfn "Result:"
printfn "%A" (numberOfEvenNumbersWithFilterAndFold [1; 2; 3; 4; 5; 6; 7; 0; 0; 0; 0])   // 7

printfn "\nList: [0; 2; 78; 4; 90; 6; 7; 52]"
printfn "Result:"
printfn "%A" (numberOfEvenNumbersWithFilterAndFold [0; 2; 78; 4; 90; 6; 7; 52])         // 7