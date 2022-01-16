{ Parsed from AppKit.framework NSDraggingItem.h }


{$ifdef TYPES}
type
  NSDraggingImageComponentPtr = ^NSDraggingImageComponent;
  NSDraggingItemPtr = ^NSDraggingItem;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSDraggingImageComponentIconKey: NSString { available in 10_7 }; cvar; external;
  NSDraggingImageComponentLabelKey: NSString { available in 10_7 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSDraggingImageComponent = objcclass external (NSObject)
  private
    _key: NSString;
    _contents: id;
    _frame: NSRect;
  public
    class function draggingImageComponentWithKey (key: NSString): NSDraggingImageComponent; message 'draggingImageComponentWithKey:';
    function initWithKey (key: NSString): instancetype; message 'initWithKey:';
    procedure setKey(newValue: NSString); message 'setKey:';
    function key: NSString; message 'key';
    procedure setContents(newValue: id); message 'setContents:';
    function contents: id; message 'contents';
    procedure setFrame(newValue: NSRect); message 'setFrame:';
    function frame: NSRect; message 'frame';
  end;

type
  NSDraggingItem = objcclass external (NSObject)
  private
    _item: id;
    _frame: NSRect;
    _components: NSArray;
    _componentsProvider: OpaqueCBlock;
    {$if  not defined(__LP64__)}
    _reserved1: NSInteger;
    _reserved2: NSInteger;
    {$endif}
  public
    function initWithPasteboardWriter (pasteboardWriter: NSPasteboardWritingProtocol): instancetype; message 'initWithPasteboardWriter:';
    function item: id; message 'item';
    procedure setDraggingFrame(newValue: NSRect); message 'setDraggingFrame:';
    function draggingFrame: NSRect; message 'draggingFrame';
    procedure setImageComponentsProvider(newValue: OpaqueCBlock); message 'setImageComponentsProvider:';
    function imageComponentsProvider: OpaqueCBlock; message 'imageComponentsProvider';
    procedure setDraggingFrame_contents (frame: NSRect; contents: id); message 'setDraggingFrame:contents:';
    function imageComponents: NSArray; message 'imageComponents';
  end;
{$endif}

