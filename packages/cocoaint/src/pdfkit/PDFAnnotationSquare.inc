{ Parsed from PDFKit.framework PDFAnnotationSquare.h }


{$ifdef TYPES}
type
  PDFAnnotationSquarePtr = ^PDFAnnotationSquare;
{$endif}

{$ifdef CLASSES}

type
  PDFAnnotationSquare = objcclass external (PDFAnnotation, NSCopyingProtocol, NSCodingProtocol)
  private
    _pdfPriv2: PDFAnnotationSquarePrivateVars;
  public
    function interiorColor: NSColor; message 'interiorColor';
    procedure setInteriorColor (color_: NSColor); message 'setInteriorColor:';
  end;
{$endif}

