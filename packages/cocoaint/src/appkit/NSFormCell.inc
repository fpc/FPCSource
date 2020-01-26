{ Parsed from AppKit.framework NSFormCell.h }


{$ifdef TYPES}
type
  NSFormCellPtr = ^NSFormCell;
{$endif}

{$ifdef CLASSES}

type
  NSFormCell = objcclass external (NSActionCell)
  private
    _titleWidth: CGFloat;
    _titleCell: id;
    _titleEndPoint: CGFloat;
    {$if defined(__LP64__)}
    _preferredTextFieldWidth: CGFloat;
    {$endif}
  public
    function initTextCell (aString: NSString): instancetype; message 'initTextCell:';
    function titleWidth (aSize: NSSize): CGFloat; message 'titleWidth:';
    procedure setTitleWidth(newValue: CGFloat); message 'setTitleWidth:';
    function titleWidth_: CGFloat; message 'titleWidth';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setTitleFont(newValue: NSFont); message 'setTitleFont:';
    function titleFont: NSFont; message 'titleFont';
    procedure setTitleAlignment(newValue: NSTextAlignment); message 'setTitleAlignment:';
    function titleAlignment: NSTextAlignment; message 'titleAlignment';
    function isOpaque: ObjCBOOL; message 'isOpaque';
    procedure setPlaceholderString(newValue: NSString); message 'setPlaceholderString:';
    function placeholderString: NSString; message 'placeholderString';
    procedure setPlaceholderAttributedString(newValue: NSAttributedString); message 'setPlaceholderAttributedString:';
    function placeholderAttributedString: NSAttributedString; message 'placeholderAttributedString';
    procedure setTitleBaseWritingDirection(newValue: NSWritingDirection); message 'setTitleBaseWritingDirection:';
    function titleBaseWritingDirection: NSWritingDirection; message 'titleBaseWritingDirection';
    procedure setPreferredTextFieldWidth(newValue: CGFloat); message 'setPreferredTextFieldWidth:';
    function preferredTextFieldWidth: CGFloat; message 'preferredTextFieldWidth';
  end;


type
  NSKeyboardUI = objccategory external (NSFormCell)
    procedure setTitleWithMnemonic (stringWithAmpersand: NSString); message 'setTitleWithMnemonic:'; deprecated 'in 10_0, 10_8';
  end;

type
  NSFormCellAttributedStringMethods = objccategory external (NSFormCell)
    procedure setAttributedTitle(newValue: NSAttributedString); message 'setAttributedTitle:';
    function attributedTitle: NSAttributedString; message 'attributedTitle';
  end;
{$endif}

