{ Parsed from Appkit.framework NSPathCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:08 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSPATHCELL_PAS_T}
{$define NSPATHCELL_PAS_T}

{ Constants }

const
  NSPathStyleStandard = 0;
  NSPathStyleNavigationBar = 1;
  NSPathStylePopUp = 2;

{ Types }
type
  NSPathStyle = NSInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPATHCELL_PAS_R}
{$define NSPATHCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPATHCELL_PAS_F}
{$define NSPATHCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPATHCELL_PAS_S}
{$define NSPATHCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPathCellDelegateProtocol = objcprotocol;
  NSPathCell = objcclass;
  NSPathCellPointer = ^NSPathCell;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPATHCELL_PAS_C}
{$define NSPATHCELL_PAS_C}

{ NSPathCell }
  NSPathCell = objcclass(NSActionCell)
  private
    __backgroundColor: NSColor;
    __borderColors: NSMutableArray;
    __cells: NSMutableArray;
    __clickedCell: NSPathComponentCell;
    __hoveredCell: NSPathComponentCell;
    __popUpButtonCell: NSPopUpButtonCell;
    __currentRect: NSRect;
    __animation: NSAnimation;
    __allowedTypes: NSArray;
    __doubleAction: SEL;
    __delegate: id;
    __piFlags: bitpacked record
        reserved: 0..((1 shl 32)-1);
      end;
    __pathStyle: NSPathStyle;
    __aux: id;
    
  public
    class function alloc: NSPathCell; message 'alloc';

    function pathStyle: NSPathStyle; message 'pathStyle';
    procedure setPathStyle(style: NSPathStyle); message 'setPathStyle:';
    function URL: NSURL; message 'URL';
    procedure setURL(URL_: NSURL); message 'setURL:';
    procedure setObjectValue(obj: id); message 'setObjectValue:';
    function allowedTypes: NSArray; message 'allowedTypes';
    procedure setAllowedTypes(allowedTypes_: NSArray); message 'setAllowedTypes:';
    function delegate: id; message 'delegate';
    procedure setDelegate(value: id); message 'setDelegate:';
    class function pathComponentCellClass: Pobjc_class; message 'pathComponentCellClass';
    function pathComponentCells: NSArray; message 'pathComponentCells';
    procedure setPathComponentCells(cells: NSArray); message 'setPathComponentCells:';
    function rectOfPathComponentCell_withFrame_inView(cell: NSPathComponentCell; frame: NSRect; view: NSView): NSRect; message 'rectOfPathComponentCell:withFrame:inView:';
    function pathComponentCellAtPoint_withFrame_inView(point: NSPoint; frame: NSRect; view: NSView): NSPathComponentCell; message 'pathComponentCellAtPoint:withFrame:inView:';
    function clickedPathComponentCell: NSPathComponentCell; message 'clickedPathComponentCell';
    procedure mouseEntered_withFrame_inView(event: NSEvent; frame: NSRect; view: NSView); message 'mouseEntered:withFrame:inView:';
    procedure mouseExited_withFrame_inView(event: NSEvent; frame: NSRect; view: NSView); message 'mouseExited:withFrame:inView:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setDoubleAction(action_: SEL); message 'setDoubleAction:';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setPlaceholderString(string_: NSString); message 'setPlaceholderString:';
    function placeholderString: NSString; message 'placeholderString';
    procedure setPlaceholderAttributedString(string_: NSAttributedString); message 'setPlaceholderAttributedString:';
    function placeholderAttributedString: NSAttributedString; message 'placeholderAttributedString';
    procedure setControlSize(size: NSControlSize); message 'setControlSize:';
  end; external;

{$endif}
{$endif}
{$ifdef PROTOCOLS}
{$ifndef NSPATHCELL_PAS_P}
{$define NSPATHCELL_PAS_P}
  
{ NSPathCellDelegate Protocol }
  NSPathCellDelegateProtocol = objcprotocol
    procedure pathCell_willDisplayOpenPanel(pathCell: NSPathCell; openPanel: NSOpenPanel); message 'pathCell:willDisplayOpenPanel:';
    procedure pathCell_willPopUpMenu(pathCell: NSPathCell; menu: NSMenu); message 'pathCell:willPopUpMenu:';
  end; external name 'NSPathCellDelegate';
{$endif}
{$endif}
