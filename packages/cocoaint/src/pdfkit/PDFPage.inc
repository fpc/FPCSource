{ Parsed from PDFKit.framework PDFPage.h }


{ Types from PDFPage }
{$ifdef TYPES}


{$endif}


{$ifdef TYPES}
type
  PDFPagePtr = ^PDFPage;
{$endif}

{$ifdef TYPES}
type
  PDFDisplayBox = NSInteger;
  PDFDisplayBoxPtr = ^PDFDisplayBox;

const
  kPDFDisplayBoxMediaBox = 0;
  kPDFDisplayBoxCropBox = 1;
  kPDFDisplayBoxBleedBox = 2;
  kPDFDisplayBoxTrimBox = 3;
  kPDFDisplayBoxArtBox = 4;
{$endif}

{$ifdef CLASSES}

type
  PDFPage = objcclass external (NSObject, NSCopyingProtocol)
  private
    _pdfPriv: PDFPagePrivateVars;
  public
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function initWithImage (image: NSImage): id; message 'initWithImage:';
    {$endif}
    function document: PDFDocument; message 'document';
    function pageRef: CGPDFPageRef; message 'pageRef';
    function label_: NSString; message 'label';
    function boundsForBox (box: PDFDisplayBox): NSRect; message 'boundsForBox:';
    procedure setBounds_forBox (bounds: NSRect; box: PDFDisplayBox); message 'setBounds:forBox:';
    function rotation: NSInteger; message 'rotation';
    procedure setRotation (angle: NSInteger); message 'setRotation:';
    function annotations: NSArray; message 'annotations';
    function displaysAnnotations: ObjCBOOL; message 'displaysAnnotations';
    procedure setDisplaysAnnotations (display: ObjCBOOL); message 'setDisplaysAnnotations:';
    procedure addAnnotation (annotation: PDFAnnotation); message 'addAnnotation:';
    procedure removeAnnotation (annotation: PDFAnnotation); message 'removeAnnotation:';
    function annotationAtPoint (point: NSPoint): PDFAnnotation; message 'annotationAtPoint:';
    procedure drawWithBox (box: PDFDisplayBox); message 'drawWithBox:';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    procedure transformContextForBox (box: PDFDisplayBox); message 'transformContextForBox:';
    {$endif}
    function numberOfCharacters: NSUInteger; message 'numberOfCharacters';
    function string_: NSString; message 'string';
    function attributedString: NSAttributedString; message 'attributedString';
    function characterBoundsAtIndex (index: NSInteger): NSRect; message 'characterBoundsAtIndex:';
    function characterIndexAtPoint (point: NSPoint): NSInteger; message 'characterIndexAtPoint:';
    function selectionForRect (rect: NSRect): PDFSelection; message 'selectionForRect:';
    function selectionForWordAtPoint (point: NSPoint): PDFSelection; message 'selectionForWordAtPoint:';
    function selectionForLineAtPoint (point: NSPoint): PDFSelection; message 'selectionForLineAtPoint:';
    function selectionFromPoint_toPoint (startPoint: NSPoint; endPoint: NSPoint): PDFSelection; message 'selectionFromPoint:toPoint:';
    function selectionForRange (range: NSRange): PDFSelection; message 'selectionForRange:';
    function dataRepresentation: NSData; message 'dataRepresentation';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

