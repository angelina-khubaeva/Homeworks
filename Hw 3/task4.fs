open Microsoft.FSharp.Core.Operators

type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Tip

let rec height tree =
    match tree with
    | Node(_, l, r) -> 1 + max (height l) (height r)
    | Tip _ -> 1

let myTree1 = Node(0, Node(1, Node(2, Tip, Tip), Node(3, Tip, Tip)), Node(4, Tip, Tip))
printfn "
              Tree1

                0          
        1              4   
   2         3      Tip Tip
Tip Tip   Tip Tip          

Result1: %A\n\n" (height myTree1)

let myTree2 = Node(0, Node(1, Node(3, Tip, Tip), Node(4, Node(6, Node(7, Tip, Tip), Tip), Tip)), Node(2, Node(5, Tip, Tip), Tip))
printfn "
              Tree2

                0             
        1              2       
   3         4              5    
                   6              
                        7             
                     Tip Tip
                       
Result2: %A" (height myTree2)