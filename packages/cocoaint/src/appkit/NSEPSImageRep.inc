{ Parsed from Appkit.framework NSEPSImageRep.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSEPSIMAGEREP_PAS_T}
{$define NSEPSIMAGEREP_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSEPSIMAGEREP_PAS_R}
{$define NSEPSIMAGEREP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSEPSIMAGEREP_PAS_F}
{$define NSEPSIMAGEREP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSEPSIMAGEREP_PAS_S}
{$define NSEPSIMAGEREP_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSEPSImageRep = objcclass;
  NSEPSImageRepPointer = ^NSEPSImageRep;
  NSEPSImageRepPtr = NSEPSImageRepPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSEPSIMAGEREP_PAS_C}
{$define NSEPSIMAGEREP_PAS_C}

{ NSEPSImageRep }
  NSEPSImageRep = objcclass(NSImageRep)
  private
    _bBoxOrigin: NSPoint;
    _epsData: NSData;
    _pdfImageRep: NSPDFImageRep;
    
  public
    class function alloc: NSEPSImageRep; message 'alloc';

    class function imageRepWithData(epsData: NSData): id; message 'imageRepWithData:';
    function initWithData(epsData: NSData): id; message 'initWithData:';
    procedure prepareGState; message 'prepareGState';
    function EPSRepresentation: NSData; message 'EPSRepresentation';
    function boundingBox: NSRect; message 'boundingBox';
  end; external;

{$endif}
{$endif}
