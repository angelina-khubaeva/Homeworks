// Реализовать класс "Очередь". При попытке получить значение из пустой очереди должно бросаться исключение. Мутабельные переменные здесь разрешены.

type public 'a List =
    | Empty                             
    | Node of 'a * 'a List              
    member this.Reverse =               // Обращение списка
        let rec reverse destList sourceList =
            match sourceList with
                 | Empty -> destList
                 | Node(sourceHead, sourceTail) -> reverse (Node(sourceHead, destList)) sourceTail
        reverse Empty this
    member this.Add x = Node(x, this)   // Добавление элемента в список
    member this.Head  =                 // Извлечение элемента из списка
        match this with
            | Empty -> failwith "Empty"
            | Node(head, tail) -> head
    member this.Tail = 
        match this with
            | Empty -> failwith "Empty"
            | Node(head, tail) -> tail

type 'a Queue (front:'a List, rear: 'a List)  =
    let frontToBack = 
        match front, rear with 
        | Empty, rear -> (rear.Reverse, Empty) 
        | _, _ -> (front, rear)
    static member Empty = Queue(List.Empty, List.Empty)     // Пустая очередь - пара пустых списков
    member this.Add value = Queue(front, rear.Add value)    // Добавление элемента в очередь
    member this.Head = 
        match frontToBack with
            | Empty, _ -> failwith "Empty"
            | List.Node(a, __), _ -> a
    member this.Tail = 
        match frontToBack with
            |Empty, _ -> failwith "Empty"
            |List.Node(a, tail), r -> Queue(tail, r)
