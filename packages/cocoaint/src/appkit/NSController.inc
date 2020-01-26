{ Parsed from AppKit.framework NSController.h }


{$ifdef TYPES}
type
  NSControllerPtr = ^NSController;
{$endif}

{$ifdef CLASSES}

type
  NSController = objcclass external (NSObject, NSCodingProtocol)
  private
    {$if  not defined(__LP64__)}
    _reserved: pointer;
    _reserved2: pointer;
    {$endif}
    _specialPurposeType: cint;
    _bindingAdaptor: id;
    _editors: NSMutableArray;
    _declaredKeys: NSMutableArray;
    _dependentKeyToModelKeyTable: NSMutableDictionary;
    _modelKeyToDependentKeyTable: NSMutableDictionary;
    _modelKeysToRefreshEachTime: NSMutableArray;
    __bindingsControllerFlags: bitpacked record
      case byte of
        0: (_anonBitField___bindingsControllerFlags0: cuint);
        1: (
          _alwaysPresentsApplicationModalAlerts: 0..1;
          _refreshesAllModelKeys: 0..1;
          _multipleObservedModelObjects: 0..1;
          _isEditing: 0..1;
          _reservedController: 0..((1 shl 28)-1);
        );
      end;
    {$if  not defined(__LP64__)}
    _reservedOther: NSMutableDictionary;
    {$endif}
    _modelObservingTracker: id;
    _expectedObservingInfo: id;
    _singleValueAccessor: id;
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    procedure objectDidBeginEditing (editor: id); message 'objectDidBeginEditing:';
    procedure objectDidEndEditing (editor: id); message 'objectDidEndEditing:';
    procedure discardEditing; message 'discardEditing';
    function commitEditing: ObjCBOOL; message 'commitEditing';
    procedure commitEditingWithDelegate_didCommitSelector_contextInfo (delegate: id; didCommitSelector: SEL; contextInfo: pointer); message 'commitEditingWithDelegate:didCommitSelector:contextInfo:';
    function isEditing: ObjCBOOL; message 'isEditing';

    { Adopted protocols }
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
  end;
{$endif}

