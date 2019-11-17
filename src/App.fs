module MidiTest

open WebMIDI

type Alert =
  | Info of string
  | Success of string
  | Warning of string
  | Error of string

type Model =  { MIDIOutputs: (string*string) list
                SelectedMIDIOutput: string option
                MIDIAccess: IMIDIAccess option
                IsMIDIEnabled: bool
                Messages: Alert list }

type Msg = 
  | MIDIConnected of IMIDIAccess     // MIDI successfully connected
  | MIDIStateChange                  // MIDI successfully connected
  | MIDIError of exn                 // Error connecting MIDI
  | Message of Alert                 // A message
  | OutputSelected of string
  | SendNote                        // Send a MIDI note

open Elmish
open Fable.React
open Fable.Core
open Browser
let init () : Model*Cmd<Msg> =
    { MIDIOutputs = []
      SelectedMIDIOutput = None
      MIDIAccess = None
      IsMIDIEnabled = false
      Messages = [] }, Cmd.OfPromise.either MIDI.requestAccess [ Sysex true ] MIDIConnected MIDIError

[<RequireQualifiedAccess>]
module JSMap =
    let toList (m: JS.Map<'key, 'value>): ('key * 'value) list =
        let mutable result = []
        m.forEach (fun value key _ -> result <- (key,value)::result) 
        result

let sendNote (midiAccess: IMIDIAccess) portId =
    let output = midiAccess.outputs.get(portId);
    
    // middle C
    let note = 60uy
    // note on, middle C, full velocity
    let noteOnMessage = [| 0x90uy; note; 0x7fuy |]
    
    // note off, middle C, release velocity = 64 
    let noteOffMessage = [| 0x80uy; note ; 0x40uy |]
    
    //omitting the timestamp means send immediately.
    output.SendAt (Browser.Performance.performance.now()) noteOnMessage   
    
    // timestamp = now + 1000ms.
    noteOffMessage |> output.SendAt (Browser.Performance.performance.now() + 1000.)

let update (msg:Msg) (model:Model) : Model*Cmd<Msg> =    
    let success = Success >> Message >> Cmd.ofMsg
    let info = Info >> Message >> Cmd.ofMsg
    let error = Error >> Message >> Cmd.ofMsg
    
    match msg with
    | MIDIConnected midiAccess -> 
        let stateChangeSub dispatch =
            midiAccess.onstatechange <- (fun (ev:IMIDIConnectionEvent) -> (dispatch MIDIStateChange))

        { model with MIDIAccess = Some midiAccess
                     IsMIDIEnabled = true }, Cmd.batch [ success "MIDI connected"
                                                         Cmd.ofSub stateChangeSub
                                                         Cmd.ofMsg MIDIStateChange ]
    | MIDIStateChange ->
        let outputs = 
            match model.MIDIAccess with
            | Some midiAccess ->
                midiAccess.outputs 
                |> JSMap.toList 
                |> List.map (fun (key, o) -> key, (o.name |> Option.defaultValue "?")) 
            | None -> []
        
        let selectedOutput = 
            match outputs with
            | (key, _)::_ -> Some key
            | _ -> None

        { model with MIDIOutputs = outputs
                     SelectedMIDIOutput = selectedOutput }, info "State changed"

    | MIDIError ex ->
        { model with MIDIAccess = None
                     MIDIOutputs = []
                     IsMIDIEnabled = false
                     SelectedMIDIOutput = None }, error ex.Message
    | Message alert -> { model with Messages = alert :: model.Messages |> List.truncate 5 }, Cmd.none
    | OutputSelected id ->
        { model with SelectedMIDIOutput = match id with 
                                          | "" -> None 
                                          | id -> Some id }, Cmd.none
    | SendNote -> 
        match model.MIDIAccess, model.SelectedMIDIOutput with
        | Some midi, Some out -> model, Cmd.OfFunc.either (sendNote midi) out (fun _ -> Message (Success "sent")) (fun ex -> Message (Error ex.Message))
        | Some _, None -> model, error "No Output"
        | _, _ -> model, error "No MIDI connection"

open Fable.React
open Fable.React
open Fable.React.Props
open Fable.React.Helpers

open Fable.Core.JsInterop

let view model dispatch =
    div [ ClassName "container" ] [
        div [ ClassName "row" ] [
            div [ ClassName "col" ] [
                div [ ClassName "card" ] [
                    div [ ClassName "card-header" ] [ strong [] [ str "MIDI Test"] ]
                    div [ ClassName "card-body" ] [
                        div [ ClassName "form-group" ] [
                            label [ ClassName "col-form-label" ] [ str "Outputs" ]
                            select [ ClassName "form-control" 
                                     Value (model.SelectedMIDIOutput |> Option.defaultValue "") 
                                     OnChange (fun (ev: Browser.Types.Event) -> dispatch (OutputSelected (!! ev.target?value))) ] [
                                         for key, name in model.MIDIOutputs do
                                            yield option [ Key key; Value key] [ str name ]
                                     ]
                        ]
                    ]
                    div [ ClassName "card-footer" ] [
                        button [ ClassName "btn btn-primary" 
                                 OnClick (fun _ -> dispatch SendNote) ] [ str "Send Note" ]
                    ]
                ]
            ]

            div [ ClassName "col" ] [
                div [ ClassName "card" ] [ 
                    div [ ClassName "card-header" ] [ strong [] [ str "MIDI Messages"] ]
                    div [ ClassName "card-body" ] [ 
                        for msg in model.Messages do
                            match msg with
                            | Info msg -> yield div [ ClassName "alert alert-info" ] [ str msg ]
                            | Success msg -> yield div [ ClassName "alert alert-success" ] [ str msg ]
                            | Warning msg -> yield div [ ClassName "alert alert-warning" ] [ str msg ]
                            | Error msg -> yield div [ ClassName "alert alert-danger" ] [ str msg ]
                    ]
                ]
            ]
        ]
    ]

open Elmish.React

Program.mkProgram init update view
|> Program.withReactBatched "midi-app"
|> Program.run