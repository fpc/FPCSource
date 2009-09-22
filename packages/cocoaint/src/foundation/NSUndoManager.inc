{ Parsed from Foundation.framework NSUndoManager.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSUNDOMANAGER_PAS_H}
{$define NSUNDOMANAGER_PAS_H}
type
  NSUndoManagerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSUNDOMANAGER_PAS_T}
{$define NSUNDOMANAGER_PAS_T}

{ Constants }

const
  NSUndoCloseGroupingRunLoopOrdering = 350000;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSUNDOMANAGER_PAS_R}
{$define NSUNDOMANAGER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSUNDOMANAGER_PAS_F}
{$define NSUNDOMANAGER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSUNDOMANAGER_PAS_T}
{$define NSUNDOMANAGER_PAS_T}

{ External string constants }
var
  NSUndoManagerCheckpointNotification: NSString; external name '_NSUndoManagerCheckpointNotification';
  NSUndoManagerWillUndoChangeNotification: NSString; external name '_NSUndoManagerWillUndoChangeNotification';
  NSUndoManagerWillRedoChangeNotification: NSString; external name '_NSUndoManagerWillRedoChangeNotification';
  NSUndoManagerDidUndoChangeNotification: NSString; external name '_NSUndoManagerDidUndoChangeNotification';
  NSUndoManagerDidRedoChangeNotification: NSString; external name '_NSUndoManagerDidRedoChangeNotification';
  NSUndoManagerDidOpenUndoGroupNotification: NSString; external name '_NSUndoManagerDidOpenUndoGroupNotification';
  NSUndoManagerWillCloseUndoGroupNotification: NSString; external name '_NSUndoManagerWillCloseUndoGroupNotification';

{$endif}
{$endif}

{$ifdef FORWARD}
  NSUndoManager = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSUNDOMANAGER_PAS_C}
{$define NSUNDOMANAGER_PAS_C}

{ NSUndoManager }
  NSUndoManager = objcclass(NSObject)
  private
    __undoStack: id;
    __redoStack: id;
    __runLoopModes: NSArray;
    __disabled: NSInteger;
    __flags: bitpacked record
        undoing: 0..1;
        redoing: 0..1;
        registeredForCallback: 0..1;
        postingCheckpointNotification: 0..1;
        groupsByEvent: 0..1;
        reserved: 0..((1 shl 27)-1);
      end;
    __target: id;
    __NSUndoManagerReserved1: Pointer;
    __NSUndoManagerReserved2: Pointer;
    __NSUndoManagerReserved3: Pointer;
    
  public
    class function alloc: NSUndoManager; message 'alloc';

    procedure beginUndoGrouping; message 'beginUndoGrouping';
    procedure endUndoGrouping; message 'endUndoGrouping';
    function groupingLevel: NSInteger; message 'groupingLevel';
    procedure disableUndoRegistration; message 'disableUndoRegistration';
    procedure enableUndoRegistration; message 'enableUndoRegistration';
    function isUndoRegistrationEnabled: Boolean; message 'isUndoRegistrationEnabled';
    function groupsByEvent: Boolean; message 'groupsByEvent';
    procedure setGroupsByEvent(groupsByEvent_: Boolean); message 'setGroupsByEvent:';
    procedure setLevelsOfUndo(levels: NSUInteger); message 'setLevelsOfUndo:';
    function levelsOfUndo: NSUInteger; message 'levelsOfUndo';
    procedure setRunLoopModes(runLoopModes_: NSArray); message 'setRunLoopModes:';
    function runLoopModes: NSArray; message 'runLoopModes';
    procedure undo; message 'undo';
    procedure redo; message 'redo';
    procedure undoNestedGroup; message 'undoNestedGroup';
    function canUndo: Boolean; message 'canUndo';
    function canRedo: Boolean; message 'canRedo';
    function isUndoing: Boolean; message 'isUndoing';
    function isRedoing: Boolean; message 'isRedoing';
    procedure removeAllActions; message 'removeAllActions';
    procedure removeAllActionsWithTarget(target: id); message 'removeAllActionsWithTarget:';
    procedure registerUndoWithTarget_selector_object(target: id; selector: SEL; anObject: id); message 'registerUndoWithTarget:selector:object:';
    function prepareWithInvocationTarget(target: id): id; message 'prepareWithInvocationTarget:';
    function undoActionName: NSString; message 'undoActionName';
    function redoActionName: NSString; message 'redoActionName';
    procedure setActionName(actionName: NSString); message 'setActionName:';
    function undoMenuItemTitle: NSString; message 'undoMenuItemTitle';
    function redoMenuItemTitle: NSString; message 'redoMenuItemTitle';
    function undoMenuTitleForUndoActionName(actionName: NSString): NSString; message 'undoMenuTitleForUndoActionName:';
    function redoMenuTitleForUndoActionName(actionName: NSString): NSString; message 'redoMenuTitleForUndoActionName:';
  end; external;

{$endif}
{$endif}
