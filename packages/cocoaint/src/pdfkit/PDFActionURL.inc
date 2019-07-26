{ Parsed from PDFKit.framework PDFActionURL.h }


{$ifdef TYPES}
type
  PDFActionURLPtr = ^PDFActionURL;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef CLASSES}

type
  PDFActionURL = objcclass external (PDFAction, NSCopyingProtocol)
  private
    _pdfPriv2: PDFActionURLPrivateVars;
  public
    function initWithURL (url: NSURL): id; message 'initWithURL:';
    function URL: NSURL; message 'URL';
    procedure setURL (url_: NSURL); message 'setURL:';
  end;
{$endif}

{$endif}
