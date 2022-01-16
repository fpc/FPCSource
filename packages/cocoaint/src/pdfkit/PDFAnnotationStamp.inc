{ Parsed from PDFKit.framework PDFAnnotationStamp.h }


{$ifdef TYPES}
type
  PDFAnnotationStampPtr = ^PDFAnnotationStamp;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef CLASSES}

type
  PDFAnnotationStamp = objcclass external (PDFAnnotation, NSCopyingProtocol)
  private
    _pdfPriv2: PDFAnnotationStampPrivateVars;
  public
    function name: NSString; message 'name';
    procedure setName (name_: NSString); message 'setName:';
  end;
{$endif}

{$endif}
