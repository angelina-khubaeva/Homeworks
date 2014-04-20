// #3.6
// Функция, генерирующая бесконечную последовательность простых чисел.

// isPrime возвращает true, если num - простое и false, если нет
let isPrime num =
    let mutable result: bool = true
    if num = 2 then true
    else
    if num % 2 = 0 then result <- false
    else 
        for i = 3 to (num / 2) do
            if num % i = 0 then result <- false
    result

let infinitSeqOfPrimes = Seq.filter (fun x -> isPrime x) (Seq.initInfinite (fun index -> index))

printfn "%A" infinitSeqOfPrimes
