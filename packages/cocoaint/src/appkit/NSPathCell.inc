{ Parsed from AppKit.framework NSPathCell.h }


{$ifdef TYPES}
type
  NSPathCellPtr = ^NSPathCell;
  NSPathCellDelegateProtocolPtr = ^NSPathCellDelegateProtocol;
{$endif}

{$ifdef TYPES}
type
  NSPathStyle = NSInteger;
  NSPathStylePtr = ^NSPathStyle;

const
  NSPathStyleStandard = 0;
  NSPathStylePopUp = 2;
  NSPathStyleNavigationBar = 1 deprecated 'in 10_5, 10_7';
{$endif}

{$ifdef CLASSES}

type
  NSPathCell = objcclass external (NSActionCell, NSOpenSavePanelDelegateProtocol)
  private
    _backgroundColor: NSColor;
    _borderColors: NSMutableArray;
    _cells: NSMutableArray;
    _clickedCell: NSPathComponentCell;
    _hoveredCell: NSPathComponentCell;
    _popUpButtonCell: NSPopUpButtonCell;
    _currentRect: NSRect;
    _animation: NSAnimation;
    _allowedTypes: NSArray;
    _doubleAction: SEL;
    _delegate: id;
    _piFlags: bitpacked record
      case byte of
        0: (_anonBitField__piFlags0: cuint);
        1: (
          cbs: 0..((1 shl 4)-1);
          reserved: 0..((1 shl 28)-1);
        );
      end;
    _pathStyle: NSPathStyle;
    _aux: id;
  public
    procedure setPathStyle(newValue: NSPathStyle); message 'setPathStyle:';
    function pathStyle: NSPathStyle; message 'pathStyle';
    procedure setURL(newValue: NSURL); message 'setURL:';
    function URL: NSURL; message 'URL';
    procedure setObjectValue (obj: NSCopyingProtocol); message 'setObjectValue:';
    procedure setAllowedTypes(newValue: NSArray); message 'setAllowedTypes:';
    function allowedTypes: NSArray; message 'allowedTypes';
    procedure setDelegate(newValue: NSPathCellDelegateProtocol); message 'setDelegate:';
    function delegate: NSPathCellDelegateProtocol; message 'delegate';
    class function pathComponentCellClass: pobjc_class; message 'pathComponentCellClass';
    procedure setPathComponentCells(newValue: NSArray); message 'setPathComponentCells:';
    function pathComponentCells: NSArray; message 'pathComponentCells';
    function rectOfPathComponentCell_withFrame_inView (cell: NSPathComponentCell; frame: NSRect; view: NSView): NSRect; message 'rectOfPathComponentCell:withFrame:inView:';
    function pathComponentCellAtPoint_withFrame_inView (point: NSPoint; frame: NSRect; view: NSView): NSPathComponentCell; message 'pathComponentCellAtPoint:withFrame:inView:';
    function clickedPathComponentCell: NSPathComponentCell; message 'clickedPathComponentCell';
    procedure mouseEntered_withFrame_inView (event: NSEvent; frame: NSRect; view: NSView); message 'mouseEntered:withFrame:inView:';
    procedure mouseExited_withFrame_inView (event: NSEvent; frame: NSRect; view: NSView); message 'mouseExited:withFrame:inView:';
    procedure setDoubleAction(newValue: SEL); message 'setDoubleAction:';
    function doubleAction: SEL; message 'doubleAction';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setPlaceholderString(newValue: NSString); message 'setPlaceholderString:';
    function placeholderString: NSString; message 'placeholderString';
    procedure setPlaceholderAttributedString(newValue: NSAttributedString); message 'setPlaceholderAttributedString:';
    function placeholderAttributedString: NSAttributedString; message 'placeholderAttributedString';

    { Adopted protocols }
    procedure panel_didChangeToDirectoryURL (sender: id; url: NSURL); message 'panel:didChangeToDirectoryURL:'; { available in 10_6 }
    function panel_shouldEnableURL (sender: id; url: NSURL): ObjCBOOL; message 'panel:shouldEnableURL:'; { available in 10_6 }
    function panel_userEnteredFilename_confirmed (sender: id; filename: NSString; okFlag: ObjCBOOL): NSString; message 'panel:userEnteredFilename:confirmed:';
    function panel_validateURL_error (sender: id; url: NSURL; outError: NSErrorPtr): ObjCBOOL; message 'panel:validateURL:error:'; { available in 10_6 }
    procedure panel_willExpand (sender: id; expanding: ObjCBOOL); message 'panel:willExpand:';
    procedure panelSelectionDidChange (sender: id); message 'panelSelectionDidChange:';
  end;
{$endif}

{$ifdef PROTOCOLS}

type
  NSPathCellDelegateProtocol = objcprotocol external name 'NSPathCellDelegate' (NSObjectProtocol)
  optional
    procedure pathCell_willDisplayOpenPanel (pathCell: NSPathCell; openPanel: NSOpenPanel); message 'pathCell:willDisplayOpenPanel:';
    procedure pathCell_willPopUpMenu (pathCell: NSPathCell; menu: NSMenu); message 'pathCell:willPopUpMenu:';
  end;
{$endif}

