{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesAutomator;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  AMAction = objcclass external;
  AMAppleScriptAction = objcclass external;
  AMBundleAction = objcclass external;
  AMShellScriptAction = objcclass external;
  AMWorkflow = objcclass external;
  AMWorkflowController = objcclass external;
  AMWorkflowView = objcclass external;

type
  NSImage = objcclass external;
  NSView = objcclass external;

implementation
end.
