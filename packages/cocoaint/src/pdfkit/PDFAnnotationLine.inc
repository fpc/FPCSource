{ Parsed from PDFKit.framework PDFAnnotationLine.h }


{ Types from PDFAnnotationLine }
{$ifdef TYPES}

{$endif}


{$ifdef TYPES}
type
  PDFAnnotationLinePtr = ^PDFAnnotationLine;
{$endif}

{$ifdef TYPES}
type
  PDFLineStyle = NSInteger;
  PDFLineStylePtr = ^PDFLineStyle;

const
  kPDFLineStyleNone = 0;
  kPDFLineStyleSquare = 1;
  kPDFLineStyleCircle = 2;
  kPDFLineStyleDiamond = 3;
  kPDFLineStyleOpenArrow = 4;
  kPDFLineStyleClosedArrow = 5;
{$endif}

{$ifdef CLASSES}

type
  PDFAnnotationLine = objcclass external (PDFAnnotation, NSCopyingProtocol, NSCodingProtocol)
  private
    _pdfPriv2: PDFAnnotationLinePrivateVars;
  public
    function startPoint: NSPoint; message 'startPoint';
    procedure setStartPoint (point: NSPoint); message 'setStartPoint:';
    function endPoint: NSPoint; message 'endPoint';
    procedure setEndPoint (point: NSPoint); message 'setEndPoint:';
    function startLineStyle: PDFLineStyle; message 'startLineStyle';
    procedure setStartLineStyle (style: PDFLineStyle); message 'setStartLineStyle:';
    function endLineStyle: PDFLineStyle; message 'endLineStyle';
    procedure setEndLineStyle (style: PDFLineStyle); message 'setEndLineStyle:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function interiorColor: NSColor; message 'interiorColor';
    procedure setInteriorColor (color_: NSColor); message 'setInteriorColor:';
    {$endif}
  end;
{$endif}

