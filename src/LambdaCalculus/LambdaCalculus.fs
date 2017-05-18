module LambdaCalculus

//Booleans
let True = fun t f -> t
let False =  fun t f -> f

//Logical operators
let Not = fun b -> b False True
let And = fun x y -> x y False
let Or = fun x y -> x True y

//If then else
let IfThenElse = fun b th el -> b th el

//Natural numbers
let Zero = fun f x -> x
let One  = fun f x -> f x
let Two  = fun f x -> f(f(x))
let Three = fun f x -> f(f(f(x)))
let Succ = fun m n -> fun s z -> ((m s) ((n s) z))

let cero = (Zero(fun x -> + 1) 0)
let uno = (One(fun x -> x + 1)cero)
let dos = (Two(fun x -> x + 1)cero)
let tres = (Three(fun x -> x + 1) cero)

//Tuples
let Tuple = fun x y -> fun f -> (f x y)
let FST = fun p -> p (fun x y -> x)
let SND = fun p -> p (fun x y -> y)

//Discriminated Unions
let INL = fun x -> fun f g -> f x
let INR = fun x -> fun f g -> g x
let MATCH = fun u -> fun f g -> ((u f ) g)

[<EntryPoint>]
let main argv =
    let var = IfThenElse
                Or True True
                    uno tres
    let tuple = Tuple "tres" 32
    let test = SND tuple
    let count = Succ Three One
    let final = count (fun x -> x + uno) cero
    let rebuildTuple = Tuple (FST (Tuple 1 2)) (SND (Tuple 1 2))
    let testMatch = MATCH (INR 21) (fun x -> x + 1) (fun y -> printfn("%A") y)
    let student = (Tuple "Stefan Koolen" 7)
    let teacher = (Tuple "Giuseppe" "happy@hoppinger.it")
    MATCH (INR (SND student)) (printfn("%s")) (printfn("%A"))
    0 // return an integer exit code
