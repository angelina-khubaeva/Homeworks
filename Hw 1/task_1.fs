let rec factorial num = 
    if num = 0 then 1 
    else num * factorial (num - 1)

printfn "Enter number:"

let n = int <| System.Console.ReadLine()

printfn "n! = %A" (factorial n)