{ Parsed from SpriteKit.framework SKMutableTexture.h }


{$ifdef TYPES}
type
  SKMutableTexturePtr = ^SKMutableTexture;
{$endif}

{$ifdef CLASSES}

type
  SKMutableTexture = objcclass external (SKTexture)
  public
    function initWithSize (size_: CGSize): instancetype; message 'initWithSize:';
    class function mutableTextureWithSize (size_: CGSize): instancetype; message 'mutableTextureWithSize:';
    function initWithSize_pixelFormat (size_: CGSize; format: cint): instancetype; message 'initWithSize:pixelFormat:';
    procedure modifyPixelDataWithBlock (block: OpaqueCBlock); message 'modifyPixelDataWithBlock:';
  end;
{$endif}

