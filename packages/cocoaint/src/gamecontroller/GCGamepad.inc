{ Parsed from GameController.framework GCGamepad.h }


{ Types from GCGamepad }
{$ifdef TYPES}


type
  GCGamepadValueChangedHandler = OpaqueCBlock;
{$endif}


{$ifdef TYPES}
type
  GCGamepadPtr = ^GCGamepad;
{$endif}

{$ifdef CLASSES}

type
  GCGamepad = objcclass external (NSObject)
  public
    {$if  not defined(__has_feature)}
    function controller: GCController; message 'controller';
    {$else}
    function controller_: GCController; message 'controller';
    {$endif}
    procedure setValueChangedHandler(newValue: GCGamepadValueChangedHandler); message 'setValueChangedHandler:';
    function valueChangedHandler: GCGamepadValueChangedHandler; message 'valueChangedHandler';
    function saveSnapshot: GCGamepadSnapshot; message 'saveSnapshot';
    function dpad: GCControllerDirectionPad; message 'dpad';
    function buttonA: GCControllerButtonInput; message 'buttonA';
    function buttonB: GCControllerButtonInput; message 'buttonB';
    function buttonX: GCControllerButtonInput; message 'buttonX';
    function buttonY: GCControllerButtonInput; message 'buttonY';
    function leftShoulder: GCControllerButtonInput; message 'leftShoulder';
    function rightShoulder: GCControllerButtonInput; message 'rightShoulder';
  end;
{$endif}

