{ Parsed from Appkit.framework NSImageCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSIMAGECELL_PAS_H}
{$define NSIMAGECELL_PAS_H}
type
  NSImageCellPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSIMAGECELL_PAS_T}
{$define NSIMAGECELL_PAS_T}

{ Constants }

const
  NSImageAlignCenter = 0;
  NSImageAlignTop = 0;
  NSImageAlignTopLeft = 1;
  NSImageAlignTopRight = 2;
  NSImageAlignLeft = 3;
  NSImageAlignBottom = 4;
  NSImageAlignBottomLeft = 5;
  NSImageAlignBottomRight = 6;
  NSImageAlignRight = 7;

const
  NSImageFrameNone = 0;
  NSImageFramePhoto = 0;
  NSImageFrameGrayBezel = 1;
  NSImageFrameGroove = 2;
  NSImageFrameButton = 3;

{ Types }
type
  NSImageAlignment = culong;
  NSImageFrameStyle = culong;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSIMAGECELL_PAS_R}
{$define NSIMAGECELL_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSIMAGECELL_PAS_F}
{$define NSIMAGECELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSIMAGECELL_PAS_T}
{$define NSIMAGECELL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSImageCell = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSIMAGECELL_PAS_C}
{$define NSIMAGECELL_PAS_C}

{ NSImageCell }
  NSImageCell = objcclass(NSCell, NSCopyingProtocol, NSCodingProtocol)
  private
    __controlView: id;
    __icFlags: bitpacked record
        _unused: 0..((1 shl 22)-1);
        _animates: 0..1;
        _align: 0..((1 shl 4)-1);
        _scale: 0..((1 shl 2)-1);
        _style: 0..((1 shl 3)-1);
      end;
    __animationState: _NSImageCellAnimationState;
    __scaledImage: NSImage;
    
  public
    class function alloc: NSImageCell; message 'alloc';

    function imageAlignment: NSImageAlignment; message 'imageAlignment';
    procedure setImageAlignment(newAlign: NSImageAlignment); message 'setImageAlignment:';
    function imageScaling: NSImageScaling; message 'imageScaling';
    procedure setImageScaling(newScaling: NSImageScaling); message 'setImageScaling:';
    function imageFrameStyle: NSImageFrameStyle; message 'imageFrameStyle';
    procedure setImageFrameStyle(newStyle: NSImageFrameStyle); message 'setImageFrameStyle:';
  end; external;

{$endif}
{$endif}
