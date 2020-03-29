{ Parsed from OSAKit.framework OSAScriptController.h }


{$ifdef TYPES}
type
  OSAScriptControllerPtr = ^OSAScriptController;
{$endif}

{$ifdef TYPES}
type
  OSAScriptState = NSInteger;
  OSAScriptStatePtr = ^OSAScriptState;

const
  OSAScriptStopped = 0;
  OSAScriptRunning = 1;
  OSAScriptRecording = 2;
{$endif}

{$ifdef CLASSES}

type
  OSAScriptController = objcclass external (NSController)
  private
    scriptView: OSAScriptView;
    resultView: NSTextView;
    _private: OSAScriptControllerPrivate;
  public
    procedure setScriptView(newValue: OSAScriptView); message 'setScriptView:';
    function scriptView_: OSAScriptView; message 'scriptView';
    procedure setResultView(newValue: NSTextView); message 'setResultView:';
    function resultView_: NSTextView; message 'resultView';
    procedure setScript(newValue: OSAScript); message 'setScript:';
    function script: OSAScript; message 'script';
    procedure setLanguage(newValue: OSALanguage); message 'setLanguage:';
    function language: OSALanguage; message 'language';
    function scriptState: OSAScriptState; message 'scriptState';
    function isCompiling: ObjCBOOL; message 'isCompiling';
    procedure compileScript (sender: id); message 'compileScript:';
    procedure recordScript (sender: id); message 'recordScript:';
    procedure runScript (sender: id); message 'runScript:';
    procedure stopScript (sender: id); message 'stopScript:';
  end;
{$endif}

