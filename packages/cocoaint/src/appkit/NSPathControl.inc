{ Parsed from AppKit.framework NSPathControl.h }


{$ifdef TYPES}
type
  NSPathControlPtr = ^NSPathControl;
  NSPathControlDelegateProtocolPtr = ^NSPathControlDelegateProtocol;
{$endif}

{$ifdef CLASSES}

type
  NSPathControl = objcclass external (NSControl)
  private
    _draggingSourceOperationMaskForLocal: NSDragOperation;
    _draggingSourceOperationMaskForNonLocal: NSDragOperation;
    _reserved: NSInteger;
    _delegate: id;
    _pathAux: id;
  public
    procedure setEditable(newValue: ObjCBOOL); message 'setEditable:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure setAllowedTypes(newValue: NSArray); message 'setAllowedTypes:';
    function allowedTypes: NSArray; message 'allowedTypes';
    procedure setPlaceholderString(newValue: NSString); message 'setPlaceholderString:';
    function placeholderString: NSString; message 'placeholderString';
    procedure setPlaceholderAttributedString(newValue: NSAttributedString); message 'setPlaceholderAttributedString:';
    function placeholderAttributedString: NSAttributedString; message 'placeholderAttributedString';
    procedure setURL(newValue: NSURL); message 'setURL:';
    function URL: NSURL; message 'URL';
    procedure setDoubleAction(newValue: SEL); message 'setDoubleAction:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setPathStyle(newValue: NSPathStyle); message 'setPathStyle:';
    function pathStyle: NSPathStyle; message 'pathStyle';
    function clickedPathItem: NSPathControlItem; message 'clickedPathItem';
    procedure setPathItems(newValue: NSArray); message 'setPathItems:';
    function pathItems: NSArray; message 'pathItems';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setDelegate(newValue: NSPathControlDelegateProtocol); message 'setDelegate:';
    function delegate: NSPathControlDelegateProtocol; message 'delegate';
    procedure setDraggingSourceOperationMask_forLocal (mask: NSDragOperation; isLocal: ObjCBOOL); message 'setDraggingSourceOperationMask:forLocal:';
    procedure setMenu(newValue: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSPathControlDelegateProtocol = objcprotocol external name 'NSPathControlDelegate' (NSObjectProtocol)
  optional
    function pathControl_shouldDragItem_withPasteboard (pathControl: NSPathControl; pathItem: NSPathControlItem; pasteboard: NSPasteboard): ObjCBOOL; message 'pathControl:shouldDragItem:withPasteboard:';
    function pathControl_shouldDragPathComponentCell_withPasteboard (pathControl: NSPathControl; pathComponentCell: NSPathComponentCell; pasteboard: NSPasteboard): ObjCBOOL; message 'pathControl:shouldDragPathComponentCell:withPasteboard:';
    function pathControl_validateDrop (pathControl: NSPathControl; info: NSDraggingInfoProtocol): NSDragOperation; message 'pathControl:validateDrop:';
    function pathControl_acceptDrop (pathControl: NSPathControl; info: NSDraggingInfoProtocol): ObjCBOOL; message 'pathControl:acceptDrop:';
    procedure pathControl_willDisplayOpenPanel (pathControl: NSPathControl; openPanel: NSOpenPanel); message 'pathControl:willDisplayOpenPanel:';
    procedure pathControl_willPopUpMenu (pathControl: NSPathControl; menu: NSMenu); message 'pathControl:willPopUpMenu:';
  end;
{$endif}

{$ifdef CLASSES}

type
  NSPathControl_NSDeprecated = objccategory external name 'NSDeprecated' (NSPathControl)
    function clickedPathComponentCell: NSPathComponentCell; message 'clickedPathComponentCell';
    function pathComponentCells: NSArray; message 'pathComponentCells';
    procedure setPathComponentCells (cells: NSArray); message 'setPathComponentCells:';
  end;
{$endif}

