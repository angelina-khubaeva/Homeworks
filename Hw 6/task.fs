// Workflow, выполняющий математические вычисления с заданной (как аргумент Builder-а) точностью.
// Например,
//     rounding 3 {
//        let! a = 2.0 / 12.0
//        let! b = 3.5
//        return a / b
//    }
//  должно возвращать 0.048

type rounding(accuracy: int) =
    member this.Bind(x: float, rest: float -> float) = rest(x)
    member this.Return(x: float) = System.Math.Round(x, accuracy)

let compute =
    rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }

printfn "result = %A" compute
