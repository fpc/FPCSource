{ Parsed from Foundation.framework NSScriptExecutionContext.h }

{$ifdef TYPES}
{$ifndef NSSCRIPTEXECUTIONCONTEXT_PAS_T}
{$define NSSCRIPTEXECUTIONCONTEXT_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCRIPTEXECUTIONCONTEXT_PAS_R}
{$define NSSCRIPTEXECUTIONCONTEXT_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCRIPTEXECUTIONCONTEXT_PAS_F}
{$define NSSCRIPTEXECUTIONCONTEXT_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCRIPTEXECUTIONCONTEXT_PAS_S}
{$define NSSCRIPTEXECUTIONCONTEXT_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScriptExecutionContext = objcclass;
  NSScriptExecutionContextPointer = ^NSScriptExecutionContext;
  NSScriptExecutionContextPtr = NSScriptExecutionContextPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTEXECUTIONCONTEXT_PAS_C}
{$define NSSCRIPTEXECUTIONCONTEXT_PAS_C}

{ NSScriptExecutionContext }
  NSScriptExecutionContext = objcclass external (NSObject)
  private
    _topLevelObject: id;
    _objectBeingTested: id;
    _rangeContainerObject: id;
    _moreVars: id;
    
  public
    class function sharedScriptExecutionContext: NSScriptExecutionContext; message 'sharedScriptExecutionContext';
    function topLevelObject: id; message 'topLevelObject';
    procedure setTopLevelObject(obj: id); message 'setTopLevelObject:';
    function objectBeingTested: id; message 'objectBeingTested';
    procedure setObjectBeingTested(obj: id); message 'setObjectBeingTested:';
    function rangeContainerObject: id; message 'rangeContainerObject';
    procedure setRangeContainerObject(obj: id); message 'setRangeContainerObject:';
  end;

{$endif}
{$endif}
