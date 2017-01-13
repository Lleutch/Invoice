namespace Invoice.Monads


module EitherMonad =
    
    type Either<'TSuccess,'TFailure> =
        |Success of 'TSuccess
        |Failure of 'TFailure
        static member FailedError (failure:Error) = 
            failure |> Failure

    and Error = 
        |Exception of string
        |NotFound of string
        |IncorrectType of string
        |IncorrectValue of string
        static member HandlerException (exp:exn) = 
            (exp.Message) |> Exception |> Failure
        
    
    type EitherBuilder() =
        member x.Bind(m,f) = 
            match m with
            |Success success -> success |> f
            |Failure failure -> Failure failure

        member x.Return(v) = Success v 
        member x.ReturnFrom(m) = m
        member x.Zero() = x.Return ()
        member x.Combine(ma,fb) =
            let mb = fb()
            match ma with
            |Success _ -> ma
            |Failure _ -> mb
        
        member x.Delay(f) = f
        
        member x.Run(f) =
            try
                x.ReturnFrom(f())
            with
            |exp -> exp |> Error.HandlerException
        
        
        member x.TryFinally(body,compensation) =
            try
                x.ReturnFrom(body)
            finally
                compensation()

    let either = EitherBuilder()

    type Either with
        static member SwitchEither (list:Either<'a,Error> list) : Either<'a list,Error> = 
            let foldFun = fun state elem -> 
                        either{
                            let! result = elem
                            let! resultState = state
                            return (result::resultState)
                        }
            list |> List.fold foldFun (Success [])