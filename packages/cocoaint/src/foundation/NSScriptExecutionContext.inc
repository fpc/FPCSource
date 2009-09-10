{ Parsed from Foundation.framework NSScriptExecutionContext.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSSCRIPTEXECUTIONCONTEXT_PAS_H}
{$define NSSCRIPTEXECUTIONCONTEXT_PAS_H}
type
  NSScriptExecutionContextPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSSCRIPTEXECUTIONCONTEXT_PAS_T}
{$define NSSCRIPTEXECUTIONCONTEXT_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScriptExecutionContext = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCRIPTEXECUTIONCONTEXT_PAS_C}
{$define NSSCRIPTEXECUTIONCONTEXT_PAS_C}

{ NSScriptExecutionContext }
  NSScriptExecutionContext = objcclass(NSObject)
  private
    __topLevelObject: id;
    __objectBeingTested: id;
    __rangeContainerObject: id;
    __moreVars: id;
    
  public
    class function alloc: NSScriptExecutionContext; message 'alloc';

    class function sharedScriptExecutionContext: NSScriptExecutionContext; message 'sharedScriptExecutionContext';
    function topLevelObject: id; message 'topLevelObject';
    procedure setTopLevelObject(obj: id); message 'setTopLevelObject:';
    function objectBeingTested: id; message 'objectBeingTested';
    procedure setObjectBeingTested(obj: id); message 'setObjectBeingTested:';
    function rangeContainerObject: id; message 'rangeContainerObject';
    procedure setRangeContainerObject(obj: id); message 'setRangeContainerObject:';
  end; external;

{$endif}
{$endif}
