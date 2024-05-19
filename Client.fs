namespace FinancialApp

open WebSharper
open WebSharper.UI
open WebSharper.UI.Html
open WebSharper.UI.Client
open WebSharper.JavaScript
open System

[<JavaScript>]
module Client =

    let math: obj = JS.Global?math
    if JS.Inline("typeof math === 'undefined'") then
        Console.Log("Math.js library is not loaded")

    let createMathInstance (): obj =
        JS.Inline("""
            math.create({
                number: 'BigNumber',
                precision: 29,
                predictable: true,
                epsilon: 1E-60
            })
        """)

    let mathInstance: obj = createMathInstance ()

    type TransactionType = 
        | Income
        | Expense

    type Category = 
        | Food 
        | Utilities 
        | Salary 
        | Entertainment
        | Others of string

    type Transaction = {
        Id: Guid
        Type: TransactionType
        Category: Category
        Amount: decimal
        Date: DateTime
    }

    type Stock = {
        Symbol: string
        Quantity: int
    }

    type Portfolio = {
        Stocks: Map<string, Stock>
    }

    let random = Random()
    let getRandomStockPrice (symbol: string): decimal = decimal(random.Next(100, 1000))

    let transactions = Var.Create(List.empty<Transaction>)
    let portfolio = Var.Create({ Stocks = Map.empty })

    let addTransaction (transaction: Transaction) =
        transactions.Value <- transaction :: transactions.Value

    let addStock (symbol: string) (quantity: int) =
        let stock =
            match portfolio.Value.Stocks.TryFind(symbol) with
            | Some s -> { s with Quantity = s.Quantity + quantity }
            | None -> { Symbol = symbol; Quantity = quantity }
        portfolio.Value <- { portfolio.Value with Stocks = portfolio.Value.Stocks.Add(symbol, stock) }

    let removeStock (symbol: string) (quantity: int) =
        match portfolio.Value.Stocks.TryFind(symbol) with
        | Some s when s.Quantity > quantity ->
            let updatedStock = { s with Quantity = s.Quantity - quantity }
            portfolio.Value <- { portfolio.Value with Stocks = portfolio.Value.Stocks.Add(symbol, updatedStock) }
        | Some s when s.Quantity = quantity ->
            portfolio.Value <- { portfolio.Value with Stocks = portfolio.Value.Stocks.Remove(symbol) }
        | _ -> ()

    let displayDashboard () =
        try
            let income = transactions.Value |> List.filter (fun t -> t.Type = Income) |> List.sumBy (fun t -> t.Amount)
            let expenses = transactions.Value |> List.filter (fun t -> t.Type = Expense) |> List.sumBy (fun t -> t.Amount)
            let netIncome = income - expenses
            div [attr.``class`` "budget-details"] [
                h3 [] [text "Current Financial Status"]
                div [attr.``class`` "totals"] [
                    
                    p [] [text $"Total Income: {income}$"]
                    p [] [text $"Total Expenses: {expenses}$"]
                    p [] [text $"Net Income: {netIncome}$"]
                ]
            ]
        with ex ->
            Console.Log("Error in displayDashboard: ", ex.Message)
            div [] [text "Error displaying dashboard"]

    let displayPortfolio () =
        div [attr.``class`` "budget-details"] [
            h3 [] [text "Current Portfolio"]
            ul [attr.``class`` "list-group"] [
                for symbol, stock in Map.toSeq portfolio.Value.Stocks do
                    let price = getRandomStockPrice symbol
                    li [attr.``class`` "list-group-item"] [text $"Symbol: {stock.Symbol}, Quantity: {stock.Quantity}, Current Price: {price}$"]
            ]
        ]

    let app () =
        let dashboard = Var.Create(displayDashboard ())
        let portfolioView = Var.Create(displayPortfolio ())

        let updateViews () =
            dashboard.Value <- displayDashboard ()
            portfolioView.Value <- displayPortfolio ()

        let transactionType = Var.Create("Income")
        let transactionCategory = Var.Create("Salary")
        let transactionAmount = Var.Create("")
        let stockSymbol = Var.Create("")
        let stockQuantity = Var.Create("")

        let addTransactionFromInput () =
            let transaction = {
                Id = Guid.NewGuid()
                Type = if transactionType.Value = "Income" then Income else Expense
                Category = 
                    match transactionCategory.Value with
                    | "Food" -> Food
                    | "Utilities" -> Utilities
                    | "Salary" -> Salary
                    | "Entertainment" -> Entertainment
                    | _ -> Others transactionCategory.Value
                Amount = decimal transactionAmount.Value
                Date = DateTime.Now
            }
            addTransaction transaction
            updateViews ()

        let addStockFromInput () =
            addStock stockSymbol.Value (int stockQuantity.Value)
            updateViews ()

        let removeStockFromInput () =
            removeStock stockSymbol.Value (int stockQuantity.Value)
            updateViews ()

        div [attr.``class`` "container"] [
            h1 [] [text "Finance Manager"]

            div [attr.``class`` "budget-form"] [
                div [attr.``class`` "add-budget"] [
                    h3 [] [text "Add Transaction"]
                    label [] [text "Type: "]
                    Doc.Select [] (fun x -> x) ["Income"; "Expense"] transactionType
                    label [] [text "Category: "]
                    Doc.Select [] (fun x -> x) ["Food"; "Utilities"; "Salary"; "Entertainment"; "Others"] transactionCategory
                    br[][]
                    label [] [text "Amount: "]
                    Doc.Input [] transactionAmount
                    button [on.click (fun _ _ -> addTransactionFromInput (); JS.Undefined)] [text "Add Transaction"]
                ]

                div [attr.``class`` "add-expense"] [
                    h3 [] [text "Manage Stocks"]
                    label [] [text "Subject: "]
                    Doc.Input [] stockSymbol
                    label [] [text "Quantity: "]
                    Doc.Input [] stockQuantity
                    button [on.click (fun _ _ -> addStockFromInput (); JS.Undefined)] [text "Add Stock"]
                    button [attr.``class`` "reset-all"; on.click (fun _ _ -> removeStockFromInput (); JS.Undefined)] [text "Remove Stock"]
                ]
            ]

            div [] [Doc.BindView id dashboard.View]
            div [] [Doc.BindView id portfolioView.View]
        ]

    [<SPAEntryPoint>]
    let Main () =
        app ()
        |> Doc.RunById "main"

module MyFix =
    [<JavaScript>]
    let SelectImpl attrs (show: 'T -> string) (optionElements) (current: Var<'T>) : Elt =
        let options = ref []
        let getIndex (el: Dom.Element) =
            el?selectedIndex : int
        let setIndex (el: Dom.Element) (i: int) =
            el?selectedIndex <- i
        let getSelectedItem el =
            let i = getIndex el
            (!options).[i]
        let itemIndex x =
            List.findIndex ((=) x) !options
        let setSelectedItem (el: Dom.Element) item =
            setIndex el (itemIndex item)
        let el = DomUtility.CreateElement "select"
        let selectedItemAttr =
            current.View
            |> Attr.DynamicCustom setSelectedItem
        let onChange (x: Dom.Event) =
            current.UpdateMaybe(fun x ->
                let y = getSelectedItem el
                if x = y then None else Some y
            )
        el.AddEventListener("change", onChange, false)
        let attrs =
            Attr.Concat attrs
            |> Attr.Append selectedItemAttr
            |> Attr.Append (Attr.OnAfterRender (fun el -> 
                setSelectedItem el <| current.Get()))
        Elt.select [attrs] (optionElements options)


    [<JavaScript>]
    let Select attrs show options current =
        let optionElements rOptions =
            rOptions := options
            options
            |> List.mapi (fun i o ->
                Doc.Element "option" [
                    Attr.Create "value" (string i)
                ] [Doc.TextNode (show o)]
                :> Doc
            )
        SelectImpl attrs show optionElements current

    [<JavaScript>]
    let SelectOptional attrs noneText show options current =
        Select attrs
            (function None -> noneText | Some x -> show x)
            (None :: List.map Some options)
            current
