{ Parsed from Appkit.framework NSFormCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSFORMCELL_PAS_H}
{$define NSFORMCELL_PAS_H}
type
  NSFormCellPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSFORMCELL_PAS_T}
{$define NSFORMCELL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSFormCell = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFORMCELL_PAS_C}
{$define NSFORMCELL_PAS_C}

{ NSFormCell }
  NSFormCell = objcclass(NSActionCell)
  private
    __titleWidth: CGFloat;
    __titleCell: id;
    __titleEndPoint: CGFloat;
    
  public
    class function alloc: NSFormCell; message 'alloc';

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

    { Category: NSKeyboardUI }
    procedure setTitleWithMnemonic(stringWithAmpersand: NSString); message 'setTitleWithMnemonic:';

    { Category: NSFormCellAttributedStringMethods }
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    procedure setAttributedTitle(obj: NSAttributedString); message 'setAttributedTitle:';
  end; external;

{$endif}
{$endif}
