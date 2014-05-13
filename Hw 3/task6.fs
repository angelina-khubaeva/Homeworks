// #3.6
// Функция, генерирующая бесконечную последовательность простых чисел.

// isPrime возвращает true, если num - простое и false, если нет
let isPrime num =
    let rec assist acc =
        if acc > num / 2 then true
        elif num % acc = 0 then false
        else assist <| acc + 1
    if num = 2 then true
    elif num % 2 = 0 then false
    else assist 3

let infinitSeqOfPrimes = Seq.filter (fun x -> isPrime x) (Seq.initInfinite (fun index -> index))

printfn "%A" infinitSeqOfPrimes