{ Parsed from PDFKit.framework PDFDestination.h }


{ Types from PDFDestination }
{$ifdef TYPES}


{$endif}


{$ifdef TYPES}
type
  PDFDestinationPtr = ^PDFDestination;
{$endif}

{$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
{$ifdef TYPES}
const
  kPDFDestinationUnspecifiedValue = FLT_MAX;
{$endif}

{$endif}
{$ifdef CLASSES}

type
  PDFDestination = objcclass external (NSObject, NSCopyingProtocol)
  private
    _pdfPriv: PDFDestinationPrivateVars;
  public
    function initWithPage_atPoint (page: PDFPage; point: NSPoint): id; message 'initWithPage:atPoint:';
    function page: PDFPage; message 'page';
    function point: NSPoint; message 'point';
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_7)}
    function zoom: CGFloat; message 'zoom';
    procedure setZoom (zoom_: CGFloat); message 'setZoom:';
    {$endif}
    {$if (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)}
    function compare (destination: PDFDestination): NSComparisonResult; message 'compare:';
    {$endif}

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
  end;
{$endif}

