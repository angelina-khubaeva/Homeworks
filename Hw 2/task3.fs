let rec digitMult n = 
    if n < 10 then n
    else digitMult (n / 10) * (n % 10)

printfn "Enter number:"

let n = int(System.Console.ReadLine())

printfn "%A" (digitMult n)