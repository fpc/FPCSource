{ Parsed from InstantMessage.framework IMAVControl.h }


{$ifdef TYPES}
type
  IMAVControlPtr = ^IMAVControl;
  IMAVButtonPtr = ^IMAVButton;
  IMAVSliderPtr = ^IMAVSlider;
  IMAVControlBarPtr = ^IMAVControlBar;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_6)}
{$ifdef CLASSES}

type
  IMAVControl = objcclass external (NSObject)
  private
    _contents: id;
  public
    function target: id; message 'target'; deprecated 'in 10_5, 10_9';
    procedure setTarget (anObject: id); message 'setTarget:'; deprecated 'in 10_5, 10_9';
    function action: SEL; message 'action'; deprecated 'in 10_5, 10_9';
    procedure setAction (aSelector: SEL); message 'setAction:'; deprecated 'in 10_5, 10_9';
    function tag: NSInteger; message 'tag'; deprecated 'in 10_5, 10_9';
    procedure setTag (anInt: NSInteger); message 'setTag:'; deprecated 'in 10_5, 10_9';
    function isEnabled: ObjCBOOL; message 'isEnabled'; deprecated 'in 10_5, 10_9';
    procedure setEnabled (flag: ObjCBOOL); message 'setEnabled:'; deprecated 'in 10_5, 10_9';
    function integerValue: NSInteger; message 'integerValue'; deprecated 'in 10_5, 10_9';
    procedure setIntegerValue (anInteger: NSInteger); message 'setIntegerValue:'; deprecated 'in 10_5, 10_9';
    function intValue: cint; message 'intValue'; deprecated 'in 10_5, 10_9';
    procedure setIntValue (anInt: cint); message 'setIntValue:'; deprecated 'in 10_5, 10_9';
    function floatValue: single; message 'floatValue'; deprecated 'in 10_5, 10_9';
    procedure setFloatValue (aFloat: single); message 'setFloatValue:'; deprecated 'in 10_5, 10_9';
    function doubleValue: double; message 'doubleValue'; deprecated 'in 10_5, 10_9';
    procedure setDoubleValue (aDouble: double); message 'setDoubleValue:'; deprecated 'in 10_5, 10_9';
  end;

type
  IMAVButton = objcclass external (IMAVControl)
  private
  public
    function state: NSInteger; message 'state'; deprecated 'in 10_5, 10_9';
    procedure setState (value: NSInteger); message 'setState:'; deprecated 'in 10_5, 10_9';
  end;


type
  IMAVButton_StandardControls = objccategory external name 'StandardControls' (IMAVButton)
    class function playPauseButton: IMAVButton; message 'playPauseButton'; deprecated 'in 10_5, 10_9';
    class function forwardButton: IMAVButton; message 'forwardButton'; deprecated 'in 10_5, 10_9';
    class function backwardButton: IMAVButton; message 'backwardButton'; deprecated 'in 10_5, 10_9';
    class function muteButton: IMAVButton; message 'muteButton'; deprecated 'in 10_5, 10_9';
  end;


type
  IMAVSlider = objcclass external (IMAVControl)
  private
  public
    function minValue: double; message 'minValue'; deprecated 'in 10_5, 10_9';
    procedure setMinValue (aDouble: double); message 'setMinValue:'; deprecated 'in 10_5, 10_9';
    function maxValue: double; message 'maxValue'; deprecated 'in 10_5, 10_9';
    procedure setMaxValue (aDouble: double); message 'setMaxValue:'; deprecated 'in 10_5, 10_9';
  end;


type
  StandardControls = objccategory external (IMAVSlider)
    class function timeSlider: IMAVSlider; message 'timeSlider'; deprecated 'in 10_5, 10_9';
  end;


type
  IMAVControlBar = objcclass external (NSObject)
  private
  public
    function controls: NSArray; message 'controls'; deprecated 'in 10_5, 10_9';
    procedure addControl (control: IMAVControl); message 'addControl:'; deprecated 'in 10_5, 10_9';
    procedure removeControl (control: IMAVControl); message 'removeControl:'; deprecated 'in 10_5, 10_9';
    procedure removeAllControls; message 'removeAllControls'; deprecated 'in 10_5, 10_9';
  end;
{$endif}

{$endif}
