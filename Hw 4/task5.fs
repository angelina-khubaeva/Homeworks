// Функция, применяющая функцию к каждому элементу двоичного дерева и возвращающая новое двоичное дерево, каждый элемент которого -
  // результат применения функции к соответствующему элементу исходного дерева (map для деревьев).

type Tree<'a> =
    | Node of 'a * Tree<'a> * Tree<'a>
    | Tip of 'a

let rec map func tree =
    match tree with
        | Tip x -> Tip(func x)
        | Node(x, l, r) -> Node((func x), (map func l), (map func r))
