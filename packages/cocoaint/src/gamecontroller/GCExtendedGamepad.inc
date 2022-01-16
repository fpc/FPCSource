{ Parsed from GameController.framework GCExtendedGamepad.h }


{ Types from GCExtendedGamepad }
{$ifdef TYPES}


type
  GCExtendedGamepadValueChangedHandler = OpaqueCBlock;
{$endif}


{$ifdef TYPES}
type
  GCExtendedGamepadPtr = ^GCExtendedGamepad;
{$endif}

{$ifdef CLASSES}

type
  GCExtendedGamepad = objcclass external (NSObject)
  public
    {$if  not defined(__has_feature)}
    function controller: GCController; message 'controller';
    {$else}
    function controller_: GCController; message 'controller';
    {$endif}
    procedure setValueChangedHandler(newValue: GCExtendedGamepadValueChangedHandler); message 'setValueChangedHandler:';
    function valueChangedHandler: GCExtendedGamepadValueChangedHandler; message 'valueChangedHandler';
    function saveSnapshot: GCExtendedGamepadSnapshot; message 'saveSnapshot';
    function dpad: GCControllerDirectionPad; message 'dpad';
    function buttonA: GCControllerButtonInput; message 'buttonA';
    function buttonB: GCControllerButtonInput; message 'buttonB';
    function buttonX: GCControllerButtonInput; message 'buttonX';
    function buttonY: GCControllerButtonInput; message 'buttonY';
    function leftThumbstick: GCControllerDirectionPad; message 'leftThumbstick';
    function rightThumbstick: GCControllerDirectionPad; message 'rightThumbstick';
    function leftShoulder: GCControllerButtonInput; message 'leftShoulder';
    function rightShoulder: GCControllerButtonInput; message 'rightShoulder';
    function leftTrigger: GCControllerButtonInput; message 'leftTrigger';
    function rightTrigger: GCControllerButtonInput; message 'rightTrigger';
  end;
{$endif}

