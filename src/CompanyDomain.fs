namespace CompanyDomain

type UserInput = {
    Mathematician: string
    Divider: string
    Divisior: string
}

type DivisionResult =
    | Int of int

type ParseError =
    | NotANumberError of string

type DivisionError =
    | DividerError of DividerError
    | DivisorError of DivisorError

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
    type private Divider =
        | Int of int

    type private Divisor =
        | NotZeroInt of int

    type private DivideIntegers = int -> int -> int

    //
    // dividing
    //

    let private divideIntegers: DivideIntegers = (/)

    let private divide divider divisor =
        match (divider, divisor) with
        | Divider.Int divider, Divisor.NotZeroInt divisor -> divideIntegers divider divisor |> DivisionResult.Int |> Ok

    //
    // parsing
    //

    let private (|Int|_|) (string: string) =
        match System.Int32.TryParse(string) with
        | (true,int) -> Some(int)
        | _ -> None

    let private parseDivider input =
        match input with
        | Int intNumber -> Ok (Divider.Int intNumber)
        | input -> input |> NotANumberError |> DividerError.ParseError |> Error

    let private parseDivisor = function
        | Int intNumber when intNumber = 0 -> Error (DivisorError.DivisionByZero)
        | Int intNumber -> Ok (Divisor.NotZeroInt intNumber)
        | input ->  input |> NotANumberError |> DivisorError.ParseError |> Error

    //
    // public api
    //

    let byUserInput log : DivideIntegersAsAService =
        fun { Mathematician = mathematicianInput; Divider = dividerInput; Divisior = divisorInput } ->
            log (sprintf "I'm counting %s / %s for %s" dividerInput divisorInput mathematicianInput)

            result {
                let! divider =
                    dividerInput
                    |> parseDivider
                    |> Result.mapError DividerError

                let! divisor =
                    divisorInput
                    |> parseDivisor
                    |> Result.mapError DivisorError

                return! divide divider divisor
            }
