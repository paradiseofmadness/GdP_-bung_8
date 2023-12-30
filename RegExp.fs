module RegExp
open Mini
open Types

//     Ausdruck            nubbalble
//A:    ab(ab)*|ba(ba)*     false
//B:    b(ab)*              false
//C:    a(ba)*              false
//D:    (ab)*               true
//E:    (ba)*               true
//F:    empty Language      false

// c)
let accept (input : List<Alphabet>): Bool =
    let rec acceptA (input: List<Alphabet>): Bool =
        match input with
            | [] -> false
            | A::rest -> acceptB rest
            | B::rest -> acceptC rest
    and acceptB (input: List<Alphabet>): Bool =
        match input with
            | [] -> false
            | A::rest -> acceptF rest
            | B::rest -> acceptD rest
    and acceptC (input: List<Alphabet>): Bool =
        match input with
            | [] -> false
            | A::rest -> acceptE rest
            | B::rest -> acceptF rest
    and acceptD (input: List<Alphabet>): Bool =
        match input with
            | [] -> true
            | A::rest -> acceptB rest
            | B::rest -> acceptF rest
    and acceptE (input: List<Alphabet>): Bool =
        match input with
            | [] -> true
            | A::rest -> acceptF rest
            | B::rest -> acceptC rest
    and acceptF (input: List<Alphabet>): Bool =
        match input with
            | [] -> false
            | A::rest -> acceptF rest
            | B::rest -> acceptF rest
    acceptA input
