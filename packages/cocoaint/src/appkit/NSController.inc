{ Parsed from Appkit.framework NSController.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Mon Sep 28 15:49:53 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCONTROLLER_PAS_H}
{$define NSCONTROLLER_PAS_H}
type
  NSControllerPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSCONTROLLER_PAS_T}
{$define NSCONTROLLER_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCONTROLLER_PAS_R}
{$define NSCONTROLLER_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCONTROLLER_PAS_F}
{$define NSCONTROLLER_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCONTROLLER_PAS_S}
{$define NSCONTROLLER_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSController = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCONTROLLER_PAS_C}
{$define NSCONTROLLER_PAS_C}

{ NSController }
  NSController = objcclass(NSObject, NSCodingProtocol)
  private
    __reserved: Pointer;
    __reserved2: Pointer;
    __specialPurposeType: cint;
    __bindingAdaptor: id;
    __editors: NSMutableArray;
    __declaredKeys: NSMutableArray;
    __dependentKeyToModelKeyTable: NSMutableDictionary;
    __modelKeyToDependentKeyTable: NSMutableDictionary;
    __modelKeysToRefreshEachTime: NSMutableArray;
    __bindingsControllerFlags: bitpacked record
        _alwaysPresentsApplicationModalAlerts: 0..1;
        _refreshesAllModelKeys: 0..1;
        _multipleObservedModelObjects: 0..1;
        _isEditing: 0..1;
        _reservedController: 0..((1 shl 28)-1);
      end;
    __reservedOther: NSMutableDictionary;
    __modelObservingTracker: id;
    __expectedObservingInfo: id;
    __singleValueAccessor: id;
    
  public
    class function alloc: NSController; message 'alloc';

    procedure objectDidBeginEditing(editor: id); message 'objectDidBeginEditing:';
    procedure objectDidEndEditing(editor: id); message 'objectDidEndEditing:';
    procedure discardEditing; message 'discardEditing';
    function commitEditing: Boolean; message 'commitEditing';
    procedure commitEditingWithDelegate_didCommitSelector_contextInfo(delegate: id; didCommitSelector: SEL; contextInfo: Pointer); message 'commitEditingWithDelegate:didCommitSelector:contextInfo:';
    function isEditing: Boolean; message 'isEditing';
  end; external;

{$endif}
{$endif}
