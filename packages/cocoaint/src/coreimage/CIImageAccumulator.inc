{ Parsed from CoreImage.framework CIImageAccumulator.h }


{$ifdef TYPES}
type
  CIImageAccumulatorPtr = ^CIImageAccumulator;
{$endif}

{$ifdef CLASSES}

type
  CIImageAccumulator = objcclass external (NSObject)
  private
    _state: pointer;
  public
    class function imageAccumulatorWithExtent_format (extent: CGRect; format: CIFormat): CIImageAccumulator; message 'imageAccumulatorWithExtent:format:';
    class function imageAccumulatorWithExtent_format_colorSpace (extent: CGRect; format: CIFormat; colorSpace: CGColorSpaceRef): CIImageAccumulator; message 'imageAccumulatorWithExtent:format:colorSpace:';
    function initWithExtent_format (extent: CGRect; format: CIFormat): id; message 'initWithExtent:format:';
    function initWithExtent_format_colorSpace (extent: CGRect; format: CIFormat; colorSpace: CGColorSpaceRef): id; message 'initWithExtent:format:colorSpace:';
    function extent: CGRect; message 'extent';
    function format: CIFormat; message 'format';
    function image: CIImage; message 'image';
    procedure setImage (im: CIImage); message 'setImage:';
    procedure setImage_dirtyRect (im: CIImage; r: CGRect); message 'setImage:dirtyRect:';
    procedure clear; message 'clear';
  end;
{$endif}

