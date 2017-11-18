namespace Rotem.Framework

module StateMachine =
    open Rotem.Framework.Common

    // Types

    type StateAction<'target, 'stateResult> = 'target -> 'stateResult
    
    type State<'target, 'stateResult> =
        {
            Name : string
            Action : StateAction<'target, 'stateResult>
        }
    
    type StateMachine<'target, 'stateResult> =
        {
            TransitionFunction : State<'target, 'stateResult> * 'stateResult -> State<'target, 'stateResult> Option
            Start : State<'target, 'stateResult>
        }

    type StateMachineConfiguration<'target, 'stateResult, 'extraData> =
        {
            StateMachine : StateMachine<'target, 'stateResult>
            PreState : State<'target, 'stateResult> -> 'extraData -> 'extraData
            PostState : State<'target, 'stateResult> -> 'stateResult -> 'extraData -> 'extraData
            StopCondition : 'target -> bool
        }

    type private StateMachineContext<'target, 'stateResult, 'extraData> =
        {
            Configuration : StateMachineConfiguration<'target, 'stateResult, 'extraData>
            Target : 'target
            State : State<'target, 'stateResult> option
            ExtraData : 'extraData
        }

    // Private functions

    let private runStateAction target state =
        state.Action target

    let private runState context state =
        let preStateExtraData = context.Configuration.PreState state context.ExtraData
        let stateResult = runStateAction context.Target state
        let postStateExtraData = context.Configuration.PostState state stateResult preStateExtraData 
        ({context with ExtraData = postStateExtraData}, stateResult)

    let private getNextState context action =
        let nextState = context.Configuration.StateMachine.TransitionFunction (Option.get context.State, action)
        {context with State = nextState}

    let private shouldStop context =
        context.Configuration.StopCondition context.Target 

    let rec private runRec context =
        match context.State with
        | Some s when shouldStop context = false -> 
            (context, s) ||> runState ||> getNextState |> runRec
        | _ -> (context.Target, context.ExtraData)

    // Public functions
    
    let (|IsState|) otherState (state, _) =
        match state with
        | {Name = name ; Action = _} when name = otherState.Name -> true
        | _ -> false
    
    let logRunningState logger state : unit =
        sprintf "Running state %s\n" state.Name
        |> logger

    let run stateMachineConfig target extraData = 
        runRec 
            { 
                Configuration = stateMachineConfig 
                Target = target 
                State =  Some stateMachineConfig.StateMachine.Start
                ExtraData = extraData
            }