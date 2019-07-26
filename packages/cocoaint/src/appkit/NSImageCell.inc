{ Parsed from AppKit.framework NSImageCell.h }


{$ifdef TYPES}
type
  NSImageCellPtr = ^NSImageCell;
{$endif}

{$ifdef TYPES}
type
  NSImageAlignment = NSUInteger;
  NSImageAlignmentPtr = ^NSImageAlignment;

const
  NSImageAlignCenter = 0;
  NSImageAlignTop = 1;
  NSImageAlignTopLeft = 2;
  NSImageAlignTopRight = 3;
  NSImageAlignLeft = 4;
  NSImageAlignBottom = 5;
  NSImageAlignBottomLeft = 6;
  NSImageAlignBottomRight = 7;
  NSImageAlignRight = 8;

type
  NSImageFrameStyle = NSUInteger;
  NSImageFrameStylePtr = ^NSImageFrameStyle;

const
  NSImageFrameNone = 0;
  NSImageFramePhoto = 1;
  NSImageFrameGrayBezel = 2;
  NSImageFrameGroove = 3;
  NSImageFrameButton = 4;
{$endif}

{$ifdef CLASSES}

type
  NSImageCell = objcclass external (NSCell, NSCopyingProtocol, NSCodingProtocol)
  private
    _controlView: id;
    __ICFlags: bitpacked record
      case byte of
        0: (_anonBitField___ICFlags0: cuint);
        1: (
          _unused: 0..((1 shl 22)-1);
          _animates: 0..1;
          _align: 0..((1 shl 4)-1);
          _scale: 0..((1 shl 2)-1);
          _style: 0..((1 shl 3)-1);
        );
      end;
    _animationState: _NSImageCellAnimationStatePtr;
    _scaledImage: NSImage;
  public
    procedure setImageAlignment(newValue: NSImageAlignment); message 'setImageAlignment:';
    function imageAlignment: NSImageAlignment; message 'imageAlignment';
    procedure setImageScaling(newValue: NSImageScaling); message 'setImageScaling:';
    function imageScaling: NSImageScaling; message 'imageScaling';
    procedure setImageFrameStyle(newValue: NSImageFrameStyle); message 'setImageFrameStyle:';
    function imageFrameStyle: NSImageFrameStyle; message 'imageFrameStyle';
  end;
{$endif}

