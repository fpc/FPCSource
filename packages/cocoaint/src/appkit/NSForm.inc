{ Parsed from AppKit.framework NSForm.h }


{$ifdef TYPES}
type
  NSFormPtr = ^NSForm;
{$endif}

{$ifdef CLASSES}

type
  NSForm = objcclass external (NSMatrix)
  public
    function indexOfSelectedItem: NSInteger; message 'indexOfSelectedItem';
    procedure setEntryWidth (width: CGFloat); message 'setEntryWidth:';
    procedure setInterlineSpacing (spacing: CGFloat); message 'setInterlineSpacing:';
    procedure setBordered (flag: ObjCBOOL); message 'setBordered:';
    procedure setBezeled (flag: ObjCBOOL); message 'setBezeled:';
    procedure setTitleAlignment (mode_: NSTextAlignment); message 'setTitleAlignment:';
    procedure setTextAlignment (mode_: NSTextAlignment); message 'setTextAlignment:';
    procedure setTitleFont (fontObj: NSFont); message 'setTitleFont:';
    procedure setTextFont (fontObj: NSFont); message 'setTextFont:';
    function cellAtIndex (index: NSInteger): id; message 'cellAtIndex:';
    procedure drawCellAtIndex (index: NSInteger); message 'drawCellAtIndex:';
    function addEntry (title: NSString): NSFormCell; message 'addEntry:';
    function insertEntry_atIndex (title: NSString; index: NSInteger): NSFormCell; message 'insertEntry:atIndex:';
    procedure removeEntryAtIndex (index: NSInteger); message 'removeEntryAtIndex:';
    function indexOfCellWithTag (aTag: NSInteger): NSInteger; message 'indexOfCellWithTag:';
    procedure selectTextAtIndex (index: NSInteger); message 'selectTextAtIndex:';
    procedure setFrameSize (newSize: NSSize); message 'setFrameSize:';
    procedure setTitleBaseWritingDirection (writingDirection: NSWritingDirection); message 'setTitleBaseWritingDirection:';
    procedure setTextBaseWritingDirection (writingDirection: NSWritingDirection); message 'setTextBaseWritingDirection:';
    procedure setPreferredTextFieldWidth (preferredWidth: CGFloat); message 'setPreferredTextFieldWidth:'; { available in 10_8 }
    function preferredTextFieldWidth: CGFloat; message 'preferredTextFieldWidth'; { available in 10_8 }
  end;
{$endif}

