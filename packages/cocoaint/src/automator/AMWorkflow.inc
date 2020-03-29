{ Parsed from Automator.framework AMWorkflow.h }


{$ifdef TYPES}
type
  AMWorkflowPtr = ^AMWorkflow;
{$endif}

{$ifdef CLASSES}

type
  AMWorkflow = objcclass external (NSObject, NSCopyingProtocol)
  private
    _uuid: NSString;
    _fileURL: NSURL;
    _actions: id;
    _variables: id;
    _connectors: id;
    _controller: AMWorkflowController;
    _options: id;
    _input: id;
    _scriptingRunData: id;
    _owningApplication: id;
    _savedPropertyList: NSDictionary;
    _loadingErrors: NSArray;
    _isBeingEdited: ObjCBOOL;
    _hasUnsavedChanges: ObjCBOOL;
    _isLoading: ObjCBOOL;
    _workflowIsShared: ObjCBOOL;
    _updateCount: NSUInteger;
    _inputType: id;
    _outputType: id;
    _future: id;
    _future2: id;
    _future3: id;
    _future4: id;
  public
    class function runWorkflowAtURL_withInput_error (fileURL: NSURL; input: id; error: NSErrorPtr): id; message 'runWorkflowAtURL:withInput:error:';
    function initWithContentsOfURL_error (fileURL: NSURL; outError: NSErrorPtr): instancetype; message 'initWithContentsOfURL:error:';
    function writeToURL_error (fileURL: NSURL; outError: NSErrorPtr): ObjCBOOL; message 'writeToURL:error:';
    function setValue_forVariableWithName (value: id; variableName: NSString): ObjCBOOL; message 'setValue:forVariableWithName:';
    function valueForVariableWithName (variableName: NSString): id; message 'valueForVariableWithName:';
    procedure addAction (action: AMAction); message 'addAction:';
    procedure removeAction (action: AMAction); message 'removeAction:';
    procedure insertAction_atIndex (action: AMAction; index: NSUInteger); message 'insertAction:atIndex:';
    procedure moveActionAtIndex_toIndex (startIndex: NSUInteger; endIndex: NSUInteger); message 'moveActionAtIndex:toIndex:';
    function fileURL: NSURL; message 'fileURL';
    function actions: NSArray; message 'actions';
    procedure setInput(newValue: id); message 'setInput:';
    function input: id; message 'input';
    function output: id; message 'output';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

