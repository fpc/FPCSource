{ Parsed from AVFoundation.framework AVAudioUnitEQ.h }


{$ifdef TYPES}
type
  AVAudioUnitEQFilterParametersPtr = ^AVAudioUnitEQFilterParameters;
  AVAudioUnitEQPtr = ^AVAudioUnitEQ;
{$endif}

{$ifdef TYPES}
type
  AVAudioUnitEQFilterType = NSInteger;
  AVAudioUnitEQFilterTypePtr = ^AVAudioUnitEQFilterType;

const
  AVAudioUnitEQFilterTypeParametric = 0;
  AVAudioUnitEQFilterTypeLowPass = 1;
  AVAudioUnitEQFilterTypeHighPass = 2;
  AVAudioUnitEQFilterTypeResonantLowPass = 3;
  AVAudioUnitEQFilterTypeResonantHighPass = 4;
  AVAudioUnitEQFilterTypeBandPass = 5;
  AVAudioUnitEQFilterTypeBandStop = 6;
  AVAudioUnitEQFilterTypeLowShelf = 7;
  AVAudioUnitEQFilterTypeHighShelf = 8;
  AVAudioUnitEQFilterTypeResonantLowShelf = 9;
  AVAudioUnitEQFilterTypeResonantHighShelf = 10;
{$endif}

{$ifdef CLASSES}

type
  AVAudioUnitEQFilterParameters = objcclass external (NSObject)
  private
    _impl: pointer;
  public
    procedure setFilterType(newValue: AVAudioUnitEQFilterType); message 'setFilterType:';
    function filterType: AVAudioUnitEQFilterType; message 'filterType';
    procedure setFrequency(newValue: single); message 'setFrequency:';
    function frequency: single; message 'frequency';
    procedure setBandwidth(newValue: single); message 'setBandwidth:';
    function bandwidth: single; message 'bandwidth';
    procedure setGain(newValue: single); message 'setGain:';
    function gain: single; message 'gain';
    procedure setBypass(newValue: ObjCBOOL); message 'setBypass:';
    function bypass: ObjCBOOL; message 'bypass';
  end;

type
  AVAudioUnitEQ = objcclass external (AVAudioUnitEffect)
  public
    function initWithNumberOfBands (numberOfBands: NSUInteger): instancetype; message 'initWithNumberOfBands:';
    function bands: NSArray; message 'bands';
    procedure setGlobalGain(newValue: single); message 'setGlobalGain:';
    function globalGain: single; message 'globalGain';
  end;
{$endif}

