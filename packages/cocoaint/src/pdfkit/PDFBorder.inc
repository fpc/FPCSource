{ Parsed from PDFKit.framework PDFBorder.h }


{$ifdef TYPES}
type
  PDFBorderPtr = ^PDFBorder;
{$endif}

{$ifdef TYPES}
type
  PDFBorderStyle = NSInteger;
  PDFBorderStylePtr = ^PDFBorderStyle;

const
  kPDFBorderStyleSolid = 0;
  kPDFBorderStyleDashed = 1;
  kPDFBorderStyleBeveled = 2;
  kPDFBorderStyleInset = 3;
  kPDFBorderStyleUnderline = 4;
{$endif}

{$ifdef CLASSES}

type
  PDFBorder = objcclass external (NSObject, NSCopyingProtocol, NSCodingProtocol)
  private
    _pdfPriv: PDFBorderPrivateVars;
  public
    function style: PDFBorderStyle; message 'style';
    procedure setStyle (style_: PDFBorderStyle); message 'setStyle:';
    function lineWidth: CGFloat; message 'lineWidth';
    procedure setLineWidth (width: CGFloat); message 'setLineWidth:';
    function dashPattern: NSArray; message 'dashPattern';
    procedure setDashPattern (pattern: NSArray); message 'setDashPattern:';
    procedure drawInRect (rect: NSRect); message 'drawInRect:';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithCoder (aDecoder: NSCoder): id; message 'initWithCoder:';
  end;
{$endif}

