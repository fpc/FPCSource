{ Parsed from GameController.framework GCControllerDirectionPad.h }


{ Types from GCControllerDirectionPad }
{$ifdef TYPES}

type
  GCControllerDirectionPadValueChangedHandler = OpaqueCBlock;
{$endif}


{$ifdef TYPES}
type
  GCControllerDirectionPadPtr = ^GCControllerDirectionPad;
{$endif}

{$ifdef CLASSES}

type
  GCControllerDirectionPad = objcclass external (GCControllerElement)
  public
    procedure setValueChangedHandler(newValue: GCControllerDirectionPadValueChangedHandler); message 'setValueChangedHandler:';
    function valueChangedHandler: GCControllerDirectionPadValueChangedHandler; message 'valueChangedHandler';
    function xAxis: GCControllerAxisInput; message 'xAxis';
    function yAxis: GCControllerAxisInput; message 'yAxis';
    function up: GCControllerButtonInput; message 'up';
    function down: GCControllerButtonInput; message 'down';
    function left: GCControllerButtonInput; message 'left';
    function right: GCControllerButtonInput; message 'right';
  end;
{$endif}

