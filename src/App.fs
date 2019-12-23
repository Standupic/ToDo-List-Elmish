module App

open Elmish
open Elmish.React
open Feliz

type TodoId = TodoId of System.Guid

type Todo =
    { 
        Id  : TodoId
        Description : string
        Completed : bool
         
    }
type State = 
    {
        Todos : Todo list
        NewTodo : string
    }

type Msg =
    | NewTodoChange of string
    | AddingNewTodo
    | DeleteTodo of TodoId


let init() =
    {
        Todos = []
        NewTodo = ""
    }

let withNewTodo description state =
    let todo = 
        {
          Id = TodoId (System.Guid.NewGuid())
          Description = description
          Completed = false 
        }

    { state with 
        Todos = List.append state.Todos [todo]
        NewTodo = ""
    }
       

let deleteTodo id state =
    let todos =
        state.Todos
        |> List.filter(fun todo -> todo.Id <> id)
    { state with Todos = todos}

let update (msg: Msg) (state: State): State  = 
    match msg with
    | NewTodoChange text ->
        { state with NewTodo = text}
    | AddingNewTodo ->
        state 
        |>
            withNewTodo state.NewTodo
    | DeleteTodo todo ->
        state
        |> deleteTodo todo



let newTodoInput (currentNewTodo: string) (dispatch: Msg -> unit) =
    Html.div [
        prop.classes [ "field"; "has-addons" ]
        prop.children[
            Html.div[
                prop.classes ["control"; "is-expanded"]
                prop.children [
                    Html.input [
                        prop.classes [ "input"; "is-medium" ]
                        prop.valueOrDefault currentNewTodo
                        prop.onTextChange (NewTodoChange >> dispatch)
                    ]
                ]
            ]
            Html.div[
                prop.classes [ "control"]
                prop.children [
                    Html.button[
                        prop.classes [ "button"; "is-primary"; "is-medium" ]
                        prop.onClick (fun _ -> AddingNewTodo |> dispatch)
                        prop.children [
                            Html.i [ prop.classes [ "fa"; "fa-plus"] ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let div ( classes: string list) ( children: Fable.React.ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]

let title = 
    Html.p [
        prop.className "title"
        prop.text "Todo List"
    ]

let renderTodo (todo: Todo) dispatch = 
    div [ "box" ] [
        div [ "columns"; "is-mobile" ] [
            div [ "column" ][
                Html.p [
                    prop.className "subtitle"
                    prop.text todo.Description
                ]
            ]
        ]
        div [ "column"; "is-narrow" ] [
            Html.button[
                prop.classes [ "button"; "is-danger" ]
                prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
                prop.children [
                     Html.i [ prop.classes [ "fa"; "fa-times" ] ]
                ]
            ]
        ]
    ]

let todoList ( todos : Todo list) dispatch =
    Html.ul [
        prop.children [ for todo in todos -> renderTodo todo dispatch ]
    ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
      prop.style [style.padding 20]
      prop.children [
          title
          newTodoInput state.NewTodo dispatch
          todoList state.Todos dispatch
      ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run