{ Parsed from PDFKit.framework PDFActionGoTo.h }


{$ifdef TYPES}
type
  PDFActionGoToPtr = ^PDFActionGoTo;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef CLASSES}

type
  PDFActionGoTo = objcclass external (PDFAction, NSCopyingProtocol)
  private
    _pdfPriv2: PDFActionGoToPrivateVars;
  public
    function initWithDestination (destination: PDFDestination): id; message 'initWithDestination:';
    function destination: PDFDestination; message 'destination';
    procedure setDestination (destination_: PDFDestination); message 'setDestination:';
  end;
{$endif}

{$endif}
