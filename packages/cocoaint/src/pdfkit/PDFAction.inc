{ Parsed from PDFKit.framework PDFAction.h }


{$ifdef TYPES}
type
  PDFActionPtr = ^PDFAction;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef CLASSES}

type
  PDFAction = objcclass external (NSObject, NSCopyingProtocol)
  private
    _pdfPriv: PDFActionPrivateVars;
  public
    function type_: NSString; message 'type';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

{$endif}
