{ Parsed from AppKit.framework NSEPSImageRep.h }


{$ifdef TYPES}
type
  NSEPSImageRepPtr = ^NSEPSImageRep;
{$endif}

{$ifdef CLASSES}

type
  NSEPSImageRep = objcclass external (NSImageRep)
  private
    _bBoxOrigin: NSPoint;
    _epsData: NSData;
    _pdfImageRep: NSPDFImageRep;
  public
    class function imageRepWithData (epsData: NSData): instancetype; message 'imageRepWithData:';
    function initWithData (epsData: NSData): instancetype; message 'initWithData:';
    procedure prepareGState; message 'prepareGState'; deprecated 'in 10_0, 10_10';
    function EPSRepresentation: NSData; message 'EPSRepresentation';
    function boundingBox: NSRect; message 'boundingBox';
  end;
{$endif}

