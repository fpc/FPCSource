{ Parsed from Appkit.framework NSPICTImageRep.h }

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
  NSPICTImageRepPtr = NSPICTImageRepPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSPICTIMAGEREP_PAS_C}
{$define NSPICTIMAGEREP_PAS_C}

{ NSPICTImageRep }
  NSPICTImageRep = objcclass external (NSImageRep)
  private
    _pictOrigin: NSPoint;
    _pictData: NSData;
    {$ifndef cpu64}
    _reserved1: cuint;
    _reserved2: cuint;
    {$else}
    _imageRep: id;
    _pictOffset: NSUInteger;
    __reserved1: cuint;
    __reserved2: cuint;
    {$endif}
    
  public
    class function imageRepWithData(pictData: NSData): id; message 'imageRepWithData:';
    function initWithData(pictData: NSData): id; message 'initWithData:';
    function PICTRepresentation: NSData; message 'PICTRepresentation';
    function boundingBox: NSRect; message 'boundingBox';
  end;

{$endif}
{$endif}
