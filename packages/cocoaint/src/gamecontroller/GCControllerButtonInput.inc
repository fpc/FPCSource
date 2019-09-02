{ Parsed from GameController.framework GCControllerButtonInput.h }


{ Types from GCControllerButtonInput }
{$ifdef TYPES}

type
  GCControllerButtonValueChangedHandler = OpaqueCBlock;
{$endif}


{$ifdef TYPES}
type
  GCControllerButtonInputPtr = ^GCControllerButtonInput;
{$endif}

{$ifdef CLASSES}

type
  GCControllerButtonInput = objcclass external (GCControllerElement)
  public
    procedure setValueChangedHandler(newValue: GCControllerButtonValueChangedHandler); message 'setValueChangedHandler:';
    function valueChangedHandler: GCControllerButtonValueChangedHandler; message 'valueChangedHandler';
    procedure setPressedChangedHandler(newValue: GCControllerButtonValueChangedHandler); message 'setPressedChangedHandler:';
    function pressedChangedHandler: GCControllerButtonValueChangedHandler; message 'pressedChangedHandler';
    function value: single; message 'value';
    function isPressed: ObjCBOOL; message 'isPressed';
  end;
{$endif}

