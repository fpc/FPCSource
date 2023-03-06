{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesScriptingBridge;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  SBApplication = objcclass external;
  SBElementArray = objcclass external;
  SBObject = objcclass external;
  SBApplicationDelegateProtocol = objcprotocol external name 'SBApplicationDelegate';

type
  SBAppContext = objcclass external;

implementation
end.
