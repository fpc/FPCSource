{ Parsed from Appkit.framework NSCIImageRep.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSCIIMAGEREP_PAS_H}
{$define NSCIIMAGEREP_PAS_H}
type
  NSCIImageRepPointer = Pointer;

{$endif}
{$endif}

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
{$ifndef NSCIIMAGEREP_PAS_T}
{$define NSCIIMAGEREP_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSCIImageRep = objcclass;

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
