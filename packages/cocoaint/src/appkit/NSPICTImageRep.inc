{ Parsed from Appkit.framework NSPICTImageRep.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSPICTIMAGEREP_PAS_T}
{$define NSPICTIMAGEREP_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSPICTIMAGEREP_PAS_R}
{$define NSPICTIMAGEREP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSPICTIMAGEREP_PAS_F}
{$define NSPICTIMAGEREP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSPICTIMAGEREP_PAS_S}
{$define NSPICTIMAGEREP_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSPICTImageRep = objcclass;
  NSPICTImageRepPointer = ^NSPICTImageRep;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPICTIMAGEREP_PAS_C}
{$define NSPICTIMAGEREP_PAS_C}

{ NSPICTImageRep }
  NSPICTImageRep = objcclass(NSImageRep)
  private
    __pictOrigin: NSPoint;
    __pictData: NSData;
    {$ifndef cpu64}
    __reserved1: cuint;
    __reserved2: cuint;
    {$else}
    __imageRep: id;
    __pictOffset: NSUInteger;
    __reserved1: cuint;
    __reserved2: cuint;
    {$endif}
    
  public
    class function alloc: NSPICTImageRep; message 'alloc';

    class function imageRepWithData(pictData: NSData): id; message 'imageRepWithData:';
    function initWithData(pictData: NSData): id; message 'initWithData:';
    function PICTRepresentation: NSData; message 'PICTRepresentation';
    function boundingBox: NSRect; message 'boundingBox';
  end; external;

{$endif}
{$endif}
