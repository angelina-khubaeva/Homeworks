let rec fibonacci n =
    match n with
    | 1 | 2 -> 1
    | _ -> fibonacci (n - 1) + fibonacci (n - 2)

let rec printFib n  =
    match n with 
    | 1 -> printf "%d " (fibonacci (n))
    | _ -> printFib (n - 1)
           printf "%d " (fibonacci (n))

printfn "Enter number:"

let num = int(System.Console.ReadLine())
           
printFib(num)