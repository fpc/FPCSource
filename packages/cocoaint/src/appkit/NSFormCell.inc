{ Parsed from Appkit.framework NSFormCell.h }

{$ifdef TYPES}
{$ifndef NSFORMCELL_PAS_T}
{$define NSFORMCELL_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSFORMCELL_PAS_R}
{$define NSFORMCELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSFORMCELL_PAS_F}
{$define NSFORMCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSFORMCELL_PAS_S}
{$define NSFORMCELL_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFormCell = objcclass;
  NSFormCellPointer = ^NSFormCell;
  NSFormCellPtr = NSFormCellPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFORMCELL_PAS_C}
{$define NSFORMCELL_PAS_C}

{ NSFormCell }
  NSFormCell = objcclass external (NSActionCell)
  private
    _titleWidth: CGFloat;
    _titleCell: id;
    _titleEndPoint: CGFloat;
    
  public
    function initTextCell(aString: NSString): id; message 'initTextCell:';
    function titleWidth(aSize: NSSize): CGFloat; message 'titleWidth:';
    procedure setTitleWidth(width: CGFloat); message 'setTitleWidth:';
    function title: NSString; message 'title';
    procedure setTitle(aString: NSString); message 'setTitle:';
    function titleFont: NSFont; message 'titleFont';
    procedure setTitleFont(fontObj: NSFont); message 'setTitleFont:';
    function titleAlignment: NSTextAlignment; message 'titleAlignment';
    procedure setTitleAlignment(mode: NSTextAlignment); message 'setTitleAlignment:';
    function isOpaque: Boolean; message 'isOpaque';
    procedure setPlaceholderString(string_: NSString); message 'setPlaceholderString:';
    function placeholderString: NSString; message 'placeholderString';
    procedure setPlaceholderAttributedString(string_: NSAttributedString); message 'setPlaceholderAttributedString:';
    function placeholderAttributedString: NSAttributedString; message 'placeholderAttributedString';
    function titleBaseWritingDirection: NSWritingDirection; message 'titleBaseWritingDirection';
    procedure setTitleBaseWritingDirection(writingDirection: NSWritingDirection); message 'setTitleBaseWritingDirection:';
  end;

{ NSKeyboardUI_NSFormCellCategory }
  NSKeyboardUI_NSFormCellCategory = objccategory external name 'NSKeyboardUI' (NSFormCell)
    procedure setTitleWithMnemonic(stringWithAmpersand: NSString); message 'setTitleWithMnemonic:';
  end;

{ NSFormCellAttributedStringMethodsCategory }
  NSFormCellAttributedStringMethodsCategory = objccategory external (NSFormCell)
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    procedure setAttributedTitle(obj: NSAttributedString); message 'setAttributedTitle:';
  end;

{$endif}
{$endif}
