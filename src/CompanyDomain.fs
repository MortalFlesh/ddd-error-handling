namespace CompanyDomain

type UserInput = {
    Mathematician: string
    Divider: string
    Divisior: string
}

type DivisionResult =
    | Int of int
    | OnlyChuckNorrisKnows

type ParseError =
    | NotANumberError of string

type DivisionError =
    | DividerError of DividerError
    | DivisorError of DivisorError
    | UnsupportedDivisionError

and DividerError =
    | ParseError of ParseError

and DivisorError =
    | ParseError of ParseError
    | DivisionByZero

type DivideIntegersAsAService = UserInput -> Result<DivisionResult, DivisionError>

module DivideInts =
    //
    // domain private types
    //
    type Mathematician =
        | Common of string
        | ChuckNorris

    module private Mathematician =
        let value = function
            | Common name -> name
            | ChuckNorris -> "🤜  Chuck Norris 🤛"

    type private Divider =
        | Int of int

    type private Divisor =
        | NotZeroInt of int
        | ZeroInt

    type private DomainDivide = Mathematician -> Divider -> Divisor -> Result<DivisionResult, DivisionError>
    type private CommonDivide = Divider -> Divisor -> Result<DivisionResult, DivisionError>
    type private DivideIntegers = int -> int -> int

    //
    // dividing
    //

    let private divideIntegers: DivideIntegers = (/)

    let private commonDivide: CommonDivide =
        fun divider divisor ->
            match (divider, divisor) with
            | _, Divisor.ZeroInt -> DivisionByZero |> DivisorError |> Error
            | Divider.Int divider, Divisor.NotZeroInt divisor -> divideIntegers divider divisor |> DivisionResult.Int |> Ok
            //| _ -> UnsupportedDivisionError |> Error

    let private divide: DomainDivide =
        fun mathematican divider divisor ->
            match mathematican with
            | Common _ -> commonDivide divider divisor
            | ChuckNorris ->
                match commonDivide divider divisor with
                | Ok result -> Ok result
                | Error e ->
                    match e with
                    | DivisorError e ->
                        match e with
                        | DivisionByZero -> Ok OnlyChuckNorrisKnows
                        | _ -> Error (DivisorError e)
                    | _ -> Error e

    //
    // parsing
    //

    let private (|ChuckNorrisName|_|) = function
        | "Chuck Norris"
        | "ChuckNorris" -> Some ChuckNorrisName
        | _ -> None

    let private (|Int|_|) (string: string) =
        match System.Int32.TryParse(string) with
        | (true,int) -> Some(int)
        | _ -> None

    let private parseDivider input =
        match input with
        | Int intNumber -> Ok (Divider.Int intNumber)
        | input -> input |> NotANumberError |> DividerError.ParseError |> Error

    let private parseDivisor = function
        | Int intNumber when intNumber = 0 -> Ok ZeroInt
        | Int intNumber -> Ok (NotZeroInt intNumber)
        | input -> input |> NotANumberError |> DivisorError.ParseError |> Error

    //
    // public api
    //

    let byUserInput log : DivideIntegersAsAService =
        fun { Mathematician = mathematicianInput; Divider = dividerInput; Divisior = divisorInput } ->
            let mathematican =
                match mathematicianInput with
                | ChuckNorrisName -> ChuckNorris
                | _ -> Common mathematicianInput

            log (sprintf "I'm counting %s / %s for %s" dividerInput divisorInput (mathematican |> Mathematician.value))

            result {
                let! divider =
                    dividerInput
                    |> parseDivider
                    |> Result.mapError DividerError

                let! divisor =
                    divisorInput
                    |> parseDivisor
                    |> Result.mapError DivisorError

                return! divide mathematican divider divisor
            }
