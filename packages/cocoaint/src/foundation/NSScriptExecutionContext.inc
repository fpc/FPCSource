{ Parsed from Foundation.framework NSScriptExecutionContext.h }


{$ifdef TYPES}
type
  NSScriptExecutionContextPtr = ^NSScriptExecutionContext;
{$endif}

{$ifdef CLASSES}

type
  NSScriptExecutionContext = objcclass external (NSObject)
  private
    _topLevelObject: id;
    _objectBeingTested: id;
    _rangeContainerObject: id;
    _moreVars: id;
  public
    class function sharedScriptExecutionContext: NSScriptExecutionContext; message 'sharedScriptExecutionContext';
    procedure setTopLevelObject(newValue: id); message 'setTopLevelObject:';
    function topLevelObject: id; message 'topLevelObject';
    procedure setObjectBeingTested(newValue: id); message 'setObjectBeingTested:';
    function objectBeingTested: id; message 'objectBeingTested';
    procedure setRangeContainerObject(newValue: id); message 'setRangeContainerObject:';
    function rangeContainerObject: id; message 'rangeContainerObject';
  end;
{$endif}

