// Функция, проверяющая, является ли строка палиндромом

let isPalindrome str = 
    let strLength = String.length str
    let rec isPalindromeAssistant n length = 
        if n >= length then printfn "It is a palindrome"
        else
            if str.[n] = str.[length] then isPalindromeAssistant (n + 1) (length - 1)
            else printfn "It is not a palindrome"
    isPalindromeAssistant 0 (strLength - 1)

printfn "Enter string:"

let str = System.Console.ReadLine()

printfn "\nResult: " 

isPalindrome str