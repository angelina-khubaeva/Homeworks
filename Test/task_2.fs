// Список произведений n1 на все числа от n1 до n2 
let rec fun2 n1 n2 =
    if n2 = n1 then [n1 * n1]
    else List.append (fun2 n1 (n2 - 1)) [n1 * n2]

// Список всех произведений двух трехзначных чисел
let rec fun1 n1 =
    if n1 = 100 then fun2 100 999
    else List.append (fun1 (n1 - 1)) (fun2 n1 999) 

let isPalindrome str = 
    let StrLength = String.length str
    let rec isPalindromeAssistant n length = 
        if n >= length then true
        else
            if str.[n] = str.[length] then isPalindromeAssistant (n + 1) (length - 1)
            else false
    isPalindromeAssistant 0 (StrLength - 1)
    
// Список всех палиндромов, полученных произведением двух трехзначных чисел
let listOfPalindromes = List.filter (fun x -> isPalindrome (string x)) (fun1 999)

// Выбор максимального числа из списка sumList
let rec maxNumber (sumList: List<int>) max = 
    if sumList.Length = 0 then max
    elif sumList.Head >= max then 
        let max1 = sumList.Head
        maxNumber sumList.Tail max1
        else maxNumber sumList.Tail max

// Результат
let num = maxNumber listOfPalindromes 999

printfn "%A" num
