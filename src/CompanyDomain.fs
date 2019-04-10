namespace CompanyDomain

type UserInput = {
    Mathematician: string
    Divider: string
    Divisior: string
}

type DivideIntegersAsAService = UserInput -> int

module DivideInts =
    type private DivideIntegers = int -> int -> int

    let divideIntegers: DivideIntegers = (/)

    let byUserInput log : DivideIntegersAsAService =
        fun { Mathematician = mathematician; Divider = divider; Divisior = divisor } ->
            log (sprintf "I'm counting %s / %s for %s" divider divisor mathematician)

            divideIntegers (int divider) (int divisor)
