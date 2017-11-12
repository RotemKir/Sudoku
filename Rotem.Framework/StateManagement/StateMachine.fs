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

    type StateMachineConfiguration<'a, 'b> =
        {
            StateMachine : StateMachine<'a, 'b>
            PreState : State<'a, 'b> -> unit
            PostState : 'b -> unit
            StopCondition : 'a -> bool
        }

    let (|IsState|) otherState (state, _)=
        match state with
        | {Name = name ; Action = _} when name = otherState.Name -> true
        | _ -> false
    
    let private runState stateMachineConfig state target=
        state |> stateMachineConfig.PreState
        let result = state.Action target
        result |> stateMachineConfig.PostState
        result

    let private getNextState stateMachineConfig currentState action =
        stateMachineConfig.StateMachine.TransitionFunction (currentState, action)

    let logRunningState logger state : unit =
        sprintf "Running state %s\n" state.Name
        |> logger

    let run stateMachineConfig target =
        let rec runRec stateMachineConfig target state =
            match state with
            | Some s when stateMachineConfig.StopCondition target = false -> 
                runState stateMachineConfig s target
                |> getNextState stateMachineConfig s
                |> runRec stateMachineConfig target
            | _ -> target
            
        runRec stateMachineConfig target <| Some stateMachineConfig.StateMachine.Start