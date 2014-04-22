let requiredFunc tree condition =

    let infix root left right = (left(); root(); right())

    let iterh trav f t = 
        let rec tr t h = 
            match t with
            | Node (x,L,R) -> trav (fun () -> (f x h)) (fun () -> tr L (h+1)) (fun () -> tr R (h+1));
            | Tip -> ()
        tr t 0
  
    let fold_infix f init t = 
        let rec tr t x = 
            match t with
            | Node (z,L,R) -> tr L (f z (tr R x))
            | Tip -> x 
        tr t init

    let treeToList T = fold_infix (fun x t -> x::t) [] T
    
    let list = treeToList tree

    let check condition_ x = 
        if condition_ x = true then [x]
        else []

    let rec createRequiredList list =
        match list with
        | [] -> list
        | h :: t -> List.append (createRequiredList list.Tail) (check condition list.Head)

    createRequiredList list