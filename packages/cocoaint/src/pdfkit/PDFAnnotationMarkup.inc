{ Parsed from PDFKit.framework PDFAnnotationMarkup.h }


{$ifdef TYPES}
type
  PDFAnnotationMarkupPtr = ^PDFAnnotationMarkup;
{$endif}

{$ifdef TYPES}
type
  PDFMarkupType = NSInteger;
  PDFMarkupTypePtr = ^PDFMarkupType;

const
  kPDFMarkupTypeHighlight = 0;
  kPDFMarkupTypeStrikeOut = 1;
  kPDFMarkupTypeUnderline = 2;
{$endif}

{$ifdef CLASSES}

type
  PDFAnnotationMarkup = objcclass external (PDFAnnotation, NSCopyingProtocol, NSCodingProtocol)
  private
    _pdfPriv2: PDFAnnotationMarkupPrivateVars;
  public
    function quadrilateralPoints: NSArray; message 'quadrilateralPoints';
    procedure setQuadrilateralPoints (points: NSArray); message 'setQuadrilateralPoints:';
    function markupType: PDFMarkupType; message 'markupType';
    procedure setMarkupType (type__: PDFMarkupType); message 'setMarkupType:';
  end;
{$endif}

