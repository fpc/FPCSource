{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesGameController;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  GCController = objcclass external;
  GCControllerAxisInput = objcclass external;
  GCControllerButtonInput = objcclass external;
  GCControllerDirectionPad = objcclass external;
  GCControllerElement = objcclass external;
  GCExtendedGamepad = objcclass external;
  GCExtendedGamepadSnapshot = objcclass external;
  GCGamepad = objcclass external;
  GCGamepadSnapshot = objcclass external;
  GCMotion = objcclass external;

implementation
end.
