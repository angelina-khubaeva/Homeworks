// #4.3
// Написать программу - телефонный справочник. Она должна уметь хранить имена и номера телефонов, в интерактивном режиме осуществлять следующие операции:
  // выйти
  // добавить запись (имя и телефон)
  // найти телефон по имени
  // сохранить текущие данные в файл
  // считать данные из файла

type States =
  | DBNew
  | DBPrev
  | Exit
  | Error

let add (db: Map<string, string>) =
    printf "Enter the name: "
    let name = System.Console.ReadLine()
    printf "Enter the phone number: "
    let number = System.Console.ReadLine()
    let alterDB =  db.Add (name, number)
    printfn "Complited"
    (DBNew, alterDB)

let exit (db: Map<string, string>) =
  (Exit, db)

let find db =
    printf "Enter the name: "
    let name = System.Console.ReadLine()
    match Map.tryFind name db with
      | None -> printfn "Nothing was found"; (DBPrev, db)
      | Some phone -> printfn "%A %A" name phone; (DBPrev, db)

let printError db =
  printfn "%s" "Invalid command"
  (Error, db)

let save db =
    printfn "Enter filename"
    let path = System.Console.ReadLine()
    let file = System.IO.File.CreateText path
    Map.iter (fun key value -> file.WriteLine (key + "|" + value)) db
    file.Close()
    printfn "File saved"
    (DBPrev, db)

let load db =
    printfn "Enter filename"
    let name = System.Console.ReadLine()
    match System.IO.File.Exists name with
      | false -> printfn "File does not exist"; (Error, db)
      | true ->
          let lines = List.ofArray <| System.IO.File.ReadAllLines name
          let line (db: Map<string, string>, str: string) =
              let parts = str.Split [|'|'|]
              db.Add (parts.[0], parts.[1])
          let alterDB = List.fold (fun acc elem -> line(acc, elem)) db lines
          printfn "Complited"
          (DBNew, alterDB)
          
let handleCommand command (db: Map<string, string>) =
  match command with
    | "e" -> exit(db)
    | "a" -> add(db)
    | "f" -> find(db)
    | "s" -> save(db)
    | "l" -> load(db)
    | _ -> printError(db)

// two statuses - "start" and "continue"
let rec work (status:string, db:Map<string, string>) =
  if status = "start" then printfn "Enter the command"
  let input = System.Console.ReadLine()
  match (handleCommand input db) with
    | (DBPrev, _) | (Error, _) -> work("continue", db)
    | (DBNew, ndb) -> work("continue", ndb)
    | (Exit, _) -> ()
      
work("start", Map.empty)
