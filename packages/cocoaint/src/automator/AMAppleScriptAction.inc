{ Parsed from Automator.framework AMAppleScriptAction.h }


{$ifdef TYPES}
type
  AMAppleScriptActionPtr = ^AMAppleScriptAction;
{$endif}

{$ifdef CLASSES}

type
  AMAppleScriptAction = objcclass external (AMBundleAction)
  private
    _script: OSAScript;
  public
    procedure setScript(newValue: OSAScript); message 'setScript:';
    function script: OSAScript; message 'script';
  end;
{$endif}

