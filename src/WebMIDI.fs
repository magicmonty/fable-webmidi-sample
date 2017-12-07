module WebMIDI

open Fable.Core
open Fable.Import

type MIDIOption =
    | Sysex of bool

[<StringEnum>]
type MIDIPortType =
    | Input
    | Output

[<StringEnum>]
type MIDIPortDeviceState =
    | Connected
    | Disconnected

[<StringEnum>]
type MIDIPortConnectionState =
    | Open
    | Closed
    | Pending

type IMIDIPort =
    inherit Browser.EventTarget
    abstract member id: string with get
    abstract member manufacturer: string option with get
    abstract member name: string option with get
    [<Emit("$0.type")>]
    abstract member Type: MIDIPortType with get
    abstract member version: string option with get
    abstract member state: MIDIPortDeviceState with get
    abstract member connection: MIDIPortConnectionState with get
    abstract member onstatechange: (IMIDIConnectionEvent -> unit) with set
    [<Emit("$0.open")>]
    abstract member Open : unit -> JS.Promise<IMIDIPort>
    abstract member close : unit -> JS.Promise<IMIDIPort>

and IMIDIConnectionEvent =
    inherit Browser.EventType
    abstract member port : IMIDIPort with get

type IMIDIOutput =
    inherit IMIDIPort
    abstract member send : byte array -> unit
    [<Emit("$0.send($2, $1)")>]
    abstract member SendAt : float -> byte array -> unit
    abstract member clear : unit -> unit

type IMIDIOutputMap = JS.Map<string, IMIDIOutput>

type IMIDIMessageEvent =
    inherit Browser.EventType
    abstract member receivedTime: double
    abstract member data: byte array
    
type IMIDIInput = 
    inherit IMIDIPort
    abstract member onmidimessage : (IMIDIMessageEvent -> unit) with set

type IMIDIInputMap = JS.Map<string, IMIDIInput>

type IMIDIAccess = 
    inherit Browser.EventTarget
    abstract member inputs : IMIDIInputMap with get
    abstract member outputs : IMIDIOutputMap with get
    abstract member onstatechange : (IMIDIConnectionEvent -> unit) with set
    abstract member sysexEnabled: bool with get

type MIDISuccessCallback = IMIDIAccess * MIDIOption -> unit

module internal Intern =

    [<Emit("navigator.requestMIDIAccess($0)")>]
    let requestAccess (options : obj) : JS.Promise<IMIDIAccess> = jsNative

open Fable.PowerPack

[<RequireQualifiedAccess>]
module MIDI =
    let requestAccess (options : MIDIOption list) : JS.Promise<IMIDIAccess> =
        Intern.requestAccess (JsInterop.keyValueList CaseRules.LowerFirst options)
