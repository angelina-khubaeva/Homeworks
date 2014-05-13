// #4.1
// Функция, которая по произвольной строке проверяет корректность скобочной последовательности в этой строке. Скобки бывают трёх видов.

let onlyBracketsStr (string: string) =
    let len = string.Length
    let rec assist length acc pos =
        if pos = len then acc
        elif string.[pos..pos] = "(" || string.[pos..pos] = ")" || string.[pos..pos] = "[" || 
             string.[pos..pos] = "]" || string.[pos..pos] = "{" || string.[pos..pos] = "}" 
            then
                assist (length - 1) (acc + string.[pos..pos]) (pos + 1)
            else
                assist (length - 1) acc (pos + 1)
    assist len "" 0

let check (bracketsStr: string) =
    let rec assist (str: string) pos acc =
        let length = String.length str
        if length = 0 then true
        elif length % 2 = 1 then false
        elif str = "()" || str = "[]" || str = "{}" then true
        elif length = 2 && not(str = "()" || str = "[]" || str = "{}") then false
        elif pos = length - 1 && not(acc >= length - 2) then assist str 0 0
        elif acc = length - 2 then false
        elif str.[pos..(pos + 1)] = "()" || str.[pos..(pos + 1)] = "[]" || str.[pos..(pos + 1)] = "{}" 
        then assist (str.[0..(pos - 1)] + str.[(pos + 2)..(length - 1)]) pos 0
        else assist str (pos + 1) (acc + 1)
    assist bracketsStr 0 0

let str1 = "d(cbabc)d([{}])"
let str = "d(c]b}a{b[c)d([{}])"

printfn "%A" <| onlyBracketsStr str1
printfn "%A" <| check (onlyBracketsStr str1)
        
printfn "%A" <| onlyBracketsStr str
printfn "%A" <| check (onlyBracketsStr str)

printfn "[] %A" <| check (onlyBracketsStr "[]")

printfn "[[ %A" <| check (onlyBracketsStr "[[")

printfn "[[[[]]]] %A" <| check (onlyBracketsStr "[[[[]]]]")
