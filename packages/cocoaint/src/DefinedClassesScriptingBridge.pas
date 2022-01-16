{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

unit DefinedClassesScriptingBridge;
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
