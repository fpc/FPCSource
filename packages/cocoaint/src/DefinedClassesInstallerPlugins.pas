{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesInstallerPlugins;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  InstallerPane = objcclass external;
  InstallerSection = objcclass external;
  InstallerState = objcclass external;

implementation
end.
