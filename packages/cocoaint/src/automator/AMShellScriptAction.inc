{ Parsed from Automator.framework AMShellScriptAction.h }


{$ifdef TYPES}
type
  AMShellScriptActionPtr = ^AMShellScriptAction;
{$endif}

{$ifdef CLASSES}

type
  AMShellScriptAction = objcclass external (AMBundleAction)
  private
  public
    function remapLineEndings: ObjCBOOL; message 'remapLineEndings';
    function inputFieldSeparator: NSString; message 'inputFieldSeparator';
    function outputFieldSeparator: NSString; message 'outputFieldSeparator';
  end;
{$endif}

