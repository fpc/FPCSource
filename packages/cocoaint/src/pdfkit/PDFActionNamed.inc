{ Parsed from PDFKit.framework PDFActionNamed.h }


{$ifdef TYPES}
type
  PDFActionNamedPtr = ^PDFActionNamed;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef TYPES}
type
  PDFActionNamedName = NSInteger;
  PDFActionNamedNamePtr = ^PDFActionNamedName;

const
  kPDFActionNamedNone = 0;
  kPDFActionNamedNextPage = 1;
  kPDFActionNamedPreviousPage = 2;
  kPDFActionNamedFirstPage = 3;
  kPDFActionNamedLastPage = 4;
  kPDFActionNamedGoBack = 5;
  kPDFActionNamedGoForward = 6;
  kPDFActionNamedGoToPage = 7;
  kPDFActionNamedFind = 8;
  kPDFActionNamedPrint = 9;
  kPDFActionNamedZoomIn = 10;
  kPDFActionNamedZoomOut = 11;
{$endif}

{$ifdef CLASSES}

type
  PDFActionNamed = objcclass external (PDFAction, NSCopyingProtocol)
  private
    _pdfPriv2: PDFActionNamedPrivateVars;
  public
    function initWithName (name: PDFActionNamedName): id; message 'initWithName:';
    function name: PDFActionNamedName; message 'name';
    procedure setName (name_: PDFActionNamedName); message 'setName:';
  end;
{$endif}

{$endif}
