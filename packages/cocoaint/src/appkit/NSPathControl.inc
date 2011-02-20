{ Parsed from Appkit.framework NSPathControl.h }

{$ifdef TYPES}
{$ifndef NSPATHCONTROL_PAS_T}
{$define NSPATHCONTROL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPATHCONTROL_PAS_R}
{$define NSPATHCONTROL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPATHCONTROL_PAS_F}
{$define NSPATHCONTROL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPATHCONTROL_PAS_S}
{$define NSPATHCONTROL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPathControlDelegateProtocol = objcprotocol;
  NSPathControl = objcclass;
  NSPathControlPointer = ^NSPathControl;
  NSPathControlPtr = NSPathControlPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPATHCONTROL_PAS_C}
{$define NSPATHCONTROL_PAS_C}

{ NSPathControl }
  NSPathControl = objcclass external (NSControl)
  private
    _draggingSourceOperationMaskForLocal: NSDragOperation;
    _draggingSourceOperationMaskForNonLocal: NSDragOperation;
    _reserved: NSInteger;
    _delegate: id;
    _aux: id;
    
  public
    function URL: NSURL; message 'URL';
    procedure setURL(url_: NSURL); message 'setURL:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setDoubleAction(action_: SEL); message 'setDoubleAction:';
    function pathStyle: NSPathStyle; message 'pathStyle';
    procedure setPathStyle(style: NSPathStyle); message 'setPathStyle:';
    function clickedPathComponentCell: NSPathComponentCell; message 'clickedPathComponentCell';
    function pathComponentCells: NSArray; message 'pathComponentCells';
    procedure setPathComponentCells(cells: NSArray); message 'setPathComponentCells:';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    function delegate: NSPathControlDelegateProtocol; message 'delegate';
    procedure setDelegate(delegate_: NSPathControlDelegateProtocol); message 'setDelegate:';
    procedure setDraggingSourceOperationMask_forLocal(mask: NSDragOperation; isLocal: Boolean); message 'setDraggingSourceOperationMask:forLocal:';
    procedure setMenu(menu_: NSMenu); message 'setMenu:';
    function menu: NSMenu; message 'menu';
  end;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSPATHCONTROL_PAS_P}
{$define NSPATHCONTROL_PAS_P}
  
{ NSPathControlDelegate Protocol }
  NSPathControlDelegateProtocol = objcprotocol external name 'NSPathControlDelegate'
  optional
    function pathControl_shouldDragPathComponentCell_withPasteboard(pathControl: NSPathControl; pathComponentCell: NSPathComponentCell; pasteboard: NSPasteboard): Boolean; message 'pathControl:shouldDragPathComponentCell:withPasteboard:';
    function pathControl_validateDrop(pathControl: NSPathControl; info: NSDraggingInfoProtocol): NSDragOperation; message 'pathControl:validateDrop:';
    function pathControl_acceptDrop(pathControl: NSPathControl; info: NSDraggingInfoProtocol): Boolean; message 'pathControl:acceptDrop:';
    procedure pathControl_willDisplayOpenPanel(pathControl: NSPathControl; openPanel: NSOpenPanel); message 'pathControl:willDisplayOpenPanel:';
    procedure pathControl_willPopUpMenu(pathControl: NSPathControl; menu: NSMenu); message 'pathControl:willPopUpMenu:';
  end;
{$endif}
{$endif}
