{ Parsed from PDFKit.framework PDFAnnotationCircle.h }


{$ifdef TYPES}
type
  PDFAnnotationCirclePtr = ^PDFAnnotationCircle;
{$endif}

{$ifdef CLASSES}

type
  PDFAnnotationCircle = objcclass external (PDFAnnotation, NSCopyingProtocol)
  private
    _pdfPriv2: PDFAnnotationCirclePrivateVars;
  public
    function interiorColor: NSColor; message 'interiorColor';
    procedure setInteriorColor (color_: NSColor); message 'setInteriorColor:';
  end;
{$endif}

