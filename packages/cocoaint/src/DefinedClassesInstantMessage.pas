{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesInstantMessage;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  IMAVButton = objcclass external;
  IMAVControl = objcclass external;
  IMAVControlBar = objcclass external;
  IMAVManager = objcclass external;
  IMAVSlider = objcclass external;
  IMService = objcclass external;

type
  ABPerson = objcclass external;

implementation
end.
