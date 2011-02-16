{ Parsed from Quartzcore.framework CIImageAccumulator.h }

{$ifdef TYPES}
{$ifndef CIIMAGEACCUMULATOR_PAS_T}
{$define CIIMAGEACCUMULATOR_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef CIIMAGEACCUMULATOR_PAS_R}
{$define CIIMAGEACCUMULATOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef CIIMAGEACCUMULATOR_PAS_F}
{$define CIIMAGEACCUMULATOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef CIIMAGEACCUMULATOR_PAS_S}
{$define CIIMAGEACCUMULATOR_PAS_S}

{$endif}
{$endif}

{$ifdef FORWARD}
  CIImageAccumulator = objcclass;
  CIImageAccumulatorPointer = ^CIImageAccumulator;
  CIImageAccumulatorPtr = CIImageAccumulatorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef CIIMAGEACCUMULATOR_PAS_C}
{$define CIIMAGEACCUMULATOR_PAS_C}

{ CIImageAccumulator }
  CIImageAccumulator = objcclass external (NSObject)
  private
    _state: Pointer;
    
  public
    class function imageAccumulatorWithExtent_format(r: CGRect; f: CIFormat): CIImageAccumulator; message 'imageAccumulatorWithExtent:format:';
    function initWithExtent_format(r: CGRect; f: CIFormat): id; message 'initWithExtent:format:';
    function extent: CGRect; message 'extent';
    function format: CIFormat; message 'format';
    function image: CIImage; message 'image';
    procedure setImage(im: CIImage); message 'setImage:';
    procedure setImage_dirtyRect(im: CIImage; r: CGRect); message 'setImage:dirtyRect:';
    procedure clear; message 'clear';
  end;

{$endif}
{$endif}
