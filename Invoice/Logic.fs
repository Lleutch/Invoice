namespace Invoice

open DomainModel

module Tooling =
    (*** Chars ***)
    let spaceChar = ' '
    let slashChar = '/'

    (*** Strings ***)
    let spaceString = spaceChar.ToString()
    let slashString = "/"
       


module ReflectionDU =
    open FSharp.Reflection
    open Monads.EitherMonad

    let getName (x: 'a) =
        either{
            let (case,_) = FSharpValue.GetUnionFields(x,typeof<'a>)
            return case.Name
        }

    let getTag (x:'a) =
        either{            
            let (case,_) = FSharpValue.GetUnionFields(x,typeof<'a>)
            return case.Tag
        }

    let getType (x:'a) =
        either{            
            let (case,_) = FSharpValue.GetUnionFields(x,typeof<'a>)
            return case.DeclaringType
        }

    let typeIsUnionType<'a> () = either{ return FSharpType.IsUnion(typeof<'a>) }

    let valueIsUnionType<'a>() = either{ return! typeIsUnionType<'a>() }

    let fromString<'a>(unionString:string) =
        either{
            match FSharpType.GetUnionCases(typeof<'a>) |> Array.filter(fun case -> case.Name=unionString) with
            |[|case|] -> return (FSharpValue.MakeUnion(case,[||]) :?> 'a)
            |_ ->   let notFoundError = 
                        (sprintf "The union type %A doesn't contain the union case %s" (typeof<'a>) unionString)
                        |> NotFound
                        |> Either<'a,Error>.FailedError 
                    return! notFoundError              
        }
    
    let toString (x:'a) =
        either{
            let! isUnionType = valueIsUnionType<'a>()  
            if isUnionType then
                let case,_ = FSharpValue.GetUnionFields(x,typeof<'a>)
                return case.Name
            else
                return! (sprintf "%A is not an union type" x) |> IncorrectType |> Either<'a,Error>.FailedError
        }    



module DomainModelExtension =
    open Tooling
    open ReflectionDU
    open Monads.EitherMonad

    type Month with
        static member FromInt(month:int) =
            either{
                match month with
                |1 -> return Month.Janvier 
                |2 -> return Month.Fevrier 
                |3 -> return Month.Mars 
                |4 -> return Month.Avril 
                |5 -> return Month.Mai 
                |6 -> return Month.Juin 
                |7 -> return Month.Juillet 
                |8 -> return Month.Aout 
                |9 -> return Month.Septembre
                |10 -> return Month.Octobre 
                |11 -> return Month.Novembre 
                |12 -> return Month.Decembre 
                |_ -> return! (sprintf "The months are tagged between 1 and 12, here you provided the value : %i" month) 
                              |> IncorrectValue
                              |> Either<Month,Error>.FailedError
            }

    type Date with
        static member FromString(date:string) =
            either{
                let datesplit = date.Split(slashChar)
                // 3 being the number of element in a Date
                if datesplit.Length = 3 then 
                    // Error handling on size of datesplit = 3
                    let! month = datesplit.[1] |> int |> Month.FromInt
                    return
                        {   Year  = datesplit.[2] |> int |> Year ;
                            Month = month;
                            Day   = datesplit.[0] |> int |> Day ;
                        }
                else
                    return! (sprintf "The date should be composed of 3 elements, and not : %i" datesplit.Length)
                            |> IncorrectValue
                            |> Either<Date,Error>.FailedError
            }

    type FirstName with
        member x.Stringify() =
            let (FirstName firstName) = x
            firstName

    type LastName with
        member x.Stringify() =
            let (LastName lastName) = x
            lastName

    type FullName with
        member x.Stringify() =
            let (FullName (firstName,lastName)) = x
            firstName.Stringify() + spaceString + lastName.Stringify()

        static member ToFullName(fullName:string) =
            let names = fullName.Split(spaceChar)
            // Error handling on the size of names = 2
            let firstName = FirstName names.[0]
            let lastName  = LastName names.[1]
            FullName (firstName,lastName)
        
        static member FromFirstandLastName (firstName:string) (lastName:string) =
            let fullname = firstName + spaceString + lastName
            FullName.ToFullName(fullname)
        
    type PaymentType with
        static member FromString(unionString) =
            unionString |> fromString<PaymentType>
            

    type ClientType with
        static member FromString(unionString) =
            unionString |> fromString<ClientType>


module Helpers =
    open DomainModelExtension
    open Monads.EitherMonad

    let convertInvoice (invoice:CsvType.Row) =
        either{
            let! date = invoice.``Date of Consultation`` |> Date.FromString 
            let consultationDate = date |> ConsultationDate
            let! paymentType = invoice.``Type of Payment`` |> PaymentType.FromString ;
            let! clientType = invoice.``Type of Client`` |> ClientType.FromString ;

            return
                {   FullName = invoice.``Last Name`` |> FullName.FromFirstandLastName (invoice.``First Name``) ;
                    Honoraire = Honoraire invoice.``Honoraire`` ;
                    InvoiceNumber = InvoiceNumber invoice.``Invoice Number`` ;
                    SocialSecurity = SocialSecurity invoice.``Social Security Number`` ;
                    ConsultationDate = consultationDate ;
                    PaymentType = paymentType;
                    ClientType = clientType ;
                }
        }

    let convert (invoices:CsvType.Row list) = 
        either{
            let! listSingleInvoice = 
                invoices 
                |> List.map(
                    fun invoice -> invoice |> convertInvoice) 
                |> Either<_,_>.SwitchEither
            return listSingleInvoice |> Invoices
        }

    let loadCsv (path:Path) =
        let (Path csvPath)  = path
        CsvType.Load(csvPath)
       
    let csvInvoices action comparable (file:CsvType) =
        [for invoice in file.Rows do
            let canBeCompared= invoice |> action
            if comparable=canBeCompared then
                yield invoice
            else
                () ]
            
    let read (path:Path) action comparable =
        path 
        |> loadCsv
        |> csvInvoices action comparable
        |> convert
    
    let csvInvoicesEither (action:CsvType.Row ->Either<'a,Error> ) comparable (file:CsvType) =
            let rec aux files acc =
                either{
                    match files with
                    |[] -> return acc |> List.rev
                    |hd::tl ->  let! canBeCompared = hd |> action
                                if comparable=canBeCompared then
                                    return! aux tl (hd::acc)
                                else
                                    return! aux tl acc
                }
            in aux (file.Rows |> List.ofSeq) []
            

    let readEither (path:Path) action comparable =
        either{
            let! invoices =
                path 
                |> loadCsv
                |> csvInvoicesEither action comparable
            return! invoices |> convert
        }
        

module readInvoices =
    open DomainModelExtension
    open Helpers
    open Monads.EitherMonad


    let readSingleFileFullName (path:Path) (fullName:FullName) =
        let action (invoice:CsvType.Row) =
            FullName.FromFirstandLastName (invoice.``First Name``) (invoice.``Last Name``)
        
        fullName |> read path action

    let readSingleFileSocialNumber (path:Path) (number:SocialSecurity) =
        let action (invoice:CsvType.Row) =
            invoice.``Social Security Number``
            |> SocialSecurity

        number |> read path action
        
    let readSingleFileDate (path:Path) (date:ConsultationDate) =
        let action (invoice:CsvType.Row) =
            either{
                let! date =
                    invoice.``Date of Consultation``
                    |> Date.FromString
                return date |> ConsultationDate
            }
            
        date |> readEither path action
        

    let readSingleFileInvoiceNumber (path:Path) (number:InvoiceNumber) =
        let (Path csvPath)  = path
        let file = CsvType.Load(csvPath)
        file.Rows
        |> List.ofSeq
        // Error handling
        |> List.find( fun invoice -> 
                        let invoiceNumber =
                            invoice.``Invoice Number``
                            |> InvoiceNumber
                        number = invoiceNumber )
        |> convertInvoice
    
    let readSingleFilePayment (path:Path) (payment:PaymentType) =
        let action (invoice:CsvType.Row) =
            invoice.``Type of Payment``
            |> PaymentType.FromString
        
        payment |> readEither path action 
        
    let readSingleFileClient (path:Path) (client:ClientType) =
        let action (invoice:CsvType.Row) =
            invoice.``Type of Client``
            |> ClientType.FromString
        
        client |> readEither path action 

    let readFromQuery (path:Path) (CsvQuery query) =
        let csvFile = path |> loadCsv
        csvFile.Rows
        |> Seq.filter(fun r -> query |> List.fold(fun acc subQuery -> acc && (subQuery r)) true)
        |> List.ofSeq
        |> convert 
               