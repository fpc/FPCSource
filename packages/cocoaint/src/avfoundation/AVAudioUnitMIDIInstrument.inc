{ Parsed from AVFoundation.framework AVAudioUnitMIDIInstrument.h }


{$ifdef TYPES}
type
  AVAudioUnitMIDIInstrumentPtr = ^AVAudioUnitMIDIInstrument;
{$endif}

{$ifdef CLASSES}

type
  AVAudioUnitMIDIInstrument = objcclass external (AVAudioUnit)
  public
    function initWithAudioComponentDescription (description: AudioComponentDescription): instancetype; message 'initWithAudioComponentDescription:';
    procedure startNote_withVelocity_onChannel (note: cuint8; velocity: cuint8; channel: cuint8); message 'startNote:withVelocity:onChannel:';
    procedure stopNote_onChannel (note: cuint8; channel: cuint8); message 'stopNote:onChannel:';
    procedure sendController_withValue_onChannel (controller: cuint8; value: cuint8; channel: cuint8); message 'sendController:withValue:onChannel:';
    procedure sendPitchBend_onChannel (pitchbend: cuint16; channel: cuint8); message 'sendPitchBend:onChannel:';
    procedure sendPressure_onChannel (pressure: cuint8; channel: cuint8); message 'sendPressure:onChannel:';
    procedure sendPressureForKey_withValue_onChannel (key: cuint8; value: cuint8; channel: cuint8); message 'sendPressureForKey:withValue:onChannel:';
    procedure sendProgramChange_onChannel (program_: cuint8; channel: cuint8); message 'sendProgramChange:onChannel:';
    procedure sendProgramChange_bankMSB_bankLSB_onChannel (program_: cuint8; bankMSB: cuint8; bankLSB: cuint8; channel: cuint8); message 'sendProgramChange:bankMSB:bankLSB:onChannel:';
    procedure sendMIDIEvent_data1_data2 (midiStatus: cuint8; data1: cuint8; data2: cuint8); message 'sendMIDIEvent:data1:data2:';
    procedure sendMIDIEvent_data1 (midiStatus: cuint8; data1: cuint8); message 'sendMIDIEvent:data1:';
    procedure sendMIDISysExEvent (midiData: NSData); message 'sendMIDISysExEvent:';
  end;
{$endif}

