{ Parsed from Appkit.framework NSPathControl.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:08 ICT 2009 }


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

{$endif}

{$ifdef CLASSES}
{$ifndef NSPATHCONTROL_PAS_C}
{$define NSPATHCONTROL_PAS_C}

{ NSPathControl }
  NSPathControl = objcclass(NSControl)
  private
    __draggingSourceOperationMaskForLocal: NSDragOperation;
    __draggingSourceOperationMaskForNonLocal: NSDragOperation;
    __dropOperation: NSDragOperation;
    __delegate: id;
    __aux: id;
    
  public
    class function alloc: NSPathControl; message 'alloc';

    function URL: NSURL; message 'URL';
    procedure setURL(URL_: NSURL); message 'setURL:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setDoubleAction(action_: SEL); message 'setDoubleAction:';
    function pathStyle: NSPathStyle; message 'pathStyle';
    procedure setPathStyle(style: NSPathStyle); message 'setPathStyle:';
    function clickedPathComponentCell: NSPathComponentCell; message 'clickedPathComponentCell';
    function pathComponentCells: NSArray; message 'pathComponentCells';
    procedure setPathComponentCells(cells: NSArray); message 'setPathComponentCells:';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    function delegate: id; message 'delegate';
    procedure setDelegate(delegate_: id); message 'setDelegate:';
    procedure setDraggingSourceOperationMask_forLocal(mask: NSDragOperation; isLocal: Boolean); message 'setDraggingSourceOperationMask:forLocal:';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSPATHCONTROL_PAS_P}
{$define NSPATHCONTROL_PAS_P}
  
{ NSPathControlDelegate Protocol }
  NSPathControlDelegateProtocol = objcprotocol
    function pathControl_shouldDragPathComponentCell_withPasteboard(pathControl: NSPathControl; pathComponentCell: NSPathComponentCell; pasteboard: NSPasteboard): Boolean; message 'pathControl:shouldDragPathComponentCell:withPasteboard:';
    function pathControl_validateDrop(pathControl: NSPathControl; info: id): NSDragOperation; message 'pathControl:validateDrop:';
    function pathControl_acceptDrop(pathControl: NSPathControl; info: id): Boolean; message 'pathControl:acceptDrop:';
    procedure pathControl_willDisplayOpenPanel(pathControl: NSPathControl; openPanel: NSOpenPanel); message 'pathControl:willDisplayOpenPanel:';
    procedure pathControl_willPopUpMenu(pathControl: NSPathControl; menu: NSMenu); message 'pathControl:willPopUpMenu:';
  end; external name 'NSPathControlDelegate';
{$endif}
{$endif}
