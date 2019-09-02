{ Parsed from PDFKit.framework PDFAnnotationInk.h }


{$ifdef TYPES}
type
  PDFAnnotationInkPtr = ^PDFAnnotationInk;
{$endif}

{$ifdef CLASSES}

type
  PDFAnnotationInk = objcclass external (PDFAnnotation, NSCopyingProtocol, NSCodingProtocol)
  private
    _pdfPriv2: PDFAnnotationInkPrivateVars;
  public
    function paths: NSArray; message 'paths';
    procedure addBezierPath (path: NSBezierPath); message 'addBezierPath:';
    procedure removeBezierPath (path: NSBezierPath); message 'removeBezierPath:';
  end;
{$endif}

