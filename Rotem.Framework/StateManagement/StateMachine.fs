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

    let run stateMachine target =
        let mutable state = Some stateMachine.Start
        let mutable action = stateMachine.Start.Action target

        while Option.isSome state do
             state <- stateMachine.TransitionFunction (Option.get state, action)
             match state with
             | Some s -> action <- s.Action target
             | _ -> ignore()

        target