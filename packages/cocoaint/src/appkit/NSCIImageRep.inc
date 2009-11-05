{ Parsed from Appkit.framework NSCIImageRep.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSCIIMAGEREP_PAS_T}
{$define NSCIIMAGEREP_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCIIMAGEREP_PAS_R}
{$define NSCIIMAGEREP_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCIIMAGEREP_PAS_F}
{$define NSCIIMAGEREP_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCIIMAGEREP_PAS_S}
{$define NSCIIMAGEREP_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCIImageRep = objcclass;
  NSCIImageRepPointer = ^NSCIImageRep;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCIIMAGEREP_PAS_C}
{$define NSCIIMAGEREP_PAS_C}

{ NSCIImageRep }
  NSCIImageRep = objcclass(NSImageRep)
  private
    __ciImage: CIImage;
    
  public
    class function alloc: NSCIImageRep; message 'alloc';

    class function imageRepWithCIImage(image: CIImage_Pointer): id; message 'imageRepWithCIImage:';
    function initWithCIImage(image: CIImage_Pointer): id; message 'initWithCIImage:';
    function CIImage: CIImage; message 'CIImage';
  end; external;

{$endif}
{$endif}
