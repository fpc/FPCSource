{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesFinderSync;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  FIFinderSync = objcclass external;
  FIFinderSyncController = objcclass external;
  FIFinderSyncProtocol = objcprotocol external name 'FIFinderSync';

implementation
end.
