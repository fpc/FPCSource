{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesMediaLibrary;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  MLMediaGroup = objcclass external;
  MLMediaLibrary = objcclass external;
  MLMediaObject = objcclass external;
  MLMediaSource = objcclass external;

type
  NSImage = objcclass external;

implementation
end.
