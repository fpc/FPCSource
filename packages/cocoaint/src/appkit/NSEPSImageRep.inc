{ Parsed from Appkit.framework NSEPSImageRep.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSEPSIMAGEREP_PAS_H}
{$define NSEPSIMAGEREP_PAS_H}
type
  NSEPSImageRepPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSEPSIMAGEREP_PAS_T}
{$define NSEPSIMAGEREP_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSEPSImageRep = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSEPSIMAGEREP_PAS_C}
{$define NSEPSIMAGEREP_PAS_C}

{ NSEPSImageRep }
  NSEPSImageRep = objcclass(NSImageRep)
  private
    __bBoxOrigin: NSPoint;
    __epsData: NSData;
    __pdfImageRep: NSPDFImageRep;
    
  public
    class function alloc: NSEPSImageRep; message 'alloc';

    class function imageRepWithData_initWithData(epsData: NSData): id; message 'imageRepWithData:';
    function initWithData(epsData: NSData): id; message 'initWithData:';
    procedure prepareGState; message 'prepareGState';
    function EPSRepresentation: NSData; message 'EPSRepresentation';
    function boundingBox: NSRect; message 'boundingBox';
  end; external;

{$endif}
{$endif}
