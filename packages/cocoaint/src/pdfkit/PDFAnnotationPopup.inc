{ Parsed from PDFKit.framework PDFAnnotationPopup.h }


{$ifdef TYPES}
type
  PDFAnnotationPopupPtr = ^PDFAnnotationPopup;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef CLASSES}

type
  PDFAnnotationPopup = objcclass external (PDFAnnotation, NSCopyingProtocol, NSCodingProtocol)
  private
    _pdfPriv2: PDFAnnotationPopupPrivateVars;
  public
    function isOpen: ObjCBOOL; message 'isOpen';
    procedure setIsOpen (isOpen_: ObjCBOOL); message 'setIsOpen:';
  end;
{$endif}

{$endif}
