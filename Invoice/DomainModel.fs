namespace Invoice

open FSharp.Data

module DomainModel =
    
    type Month =
        |Janvier
        |Fevrier
        |Mars
        |Avril
        |Mai
        |Juin
        |Juillet
        |Aout
        |Septembre
        |Octobre
        |Novembre
        |Decembre
          
    type Year = Year of int
    type Day  = Day of int

    type Date =
          { Year: Year;
            Month: Month;
            Day: Day; }

    type FirstName          = FirstName of string
    type LastName           = LastName of string

    type FullName           = FullName of FirstName*LastName
    type Honoraire          = Honoraire of int
    type InvoiceNumber      = InvoiceNumber of int
    type SocialSecurity     = SocialSecurity of int64
    type ConsultationDate   = ConsultationDate of Date

    type PaymentType =
        |Cheque
        |Espece

    type ClientType =
        |Maman
        |Bebe
        |PersonneAge

    type SingleInvoice = 
                   { FullName: FullName;
                     Honoraire: Honoraire;
                     InvoiceNumber: InvoiceNumber;
                     SocialSecurity: SocialSecurity;
                     ConsultationDate: ConsultationDate;
                     PaymentType : PaymentType;
                     ClientType : ClientType; }

    type Invoices = Invoices of SingleInvoice list
    
    type Path = Path of string

    type CsvType = CsvProvider<"..\InvoiceDatabase\Metadata\Metadata.csv">


    type CsvQuery = CsvQuery of (CsvType.Row->bool) list