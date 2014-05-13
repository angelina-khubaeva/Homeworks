let degreesOfTwo n =
    let list = [n, n * 2, n * 4, n * 8, n * 16]
    list

printfn "Enter 2^k:"

let n = int <| System.Console.ReadLine()

printfn "%A" <| degreesOfTwo n