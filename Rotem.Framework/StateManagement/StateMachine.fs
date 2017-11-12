namespace Rotem.Framework

module StateMachine =

    // Types

    type StateAction<'a, 'b> = 'a -> 'b
    
    type State<'a, 'b> =
        {
            Name : string
            Action : StateAction<'a, 'b>
        }
    
    type StateMachine<'a, 'b> =
        {
            TransitionFunction : State<'a, 'b> * 'b -> State<'a, 'b> Option
            Start : State<'a, 'b>
        }

    let (|IsState|) otherState (state, _)=
        match state with
        | {Name = name ; Action = _} when name = otherState.Name -> true
        | _ -> false
    
    let private runState preState postState state target=
        state |> preState
        let result = state.Action target
        result |> postState
        result

    let logRunningState logger state : unit =
        sprintf "Running state %s\n" state.Name
        |> logger

    let run stateMachine preState postState stopCondition target =
        let mutable state = Some stateMachine.Start
        let mutable action = runState preState postState stateMachine.Start target

        while Option.isSome state && stopCondition target = false do
             state <- stateMachine.TransitionFunction (Option.get state, action)
             match state with
             | Some s -> action <- runState preState postState s target
             | _ -> ignore()

        target