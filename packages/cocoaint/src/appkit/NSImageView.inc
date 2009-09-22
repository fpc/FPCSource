{ Parsed from Appkit.framework NSImageView.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSIMAGEVIEW_PAS_H}
{$define NSIMAGEVIEW_PAS_H}
type
  NSImageViewPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSIMAGEVIEW_PAS_T}
{$define NSIMAGEVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSIMAGEVIEW_PAS_R}
{$define NSIMAGEVIEW_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSIMAGEVIEW_PAS_F}
{$define NSIMAGEVIEW_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSIMAGEVIEW_PAS_T}
{$define NSIMAGEVIEW_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSImageView = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSIMAGEVIEW_PAS_C}
{$define NSIMAGEVIEW_PAS_C}

{ NSImageView }
  NSImageView = objcclass(NSControl)
  private
    __ivFlags: bitpacked record
        _unused: 0..((1 shl 27)-1);
        _compatibleScalingAndAlignment: 0..1;
        _compatibleImage: 0..1;
        _overridesDrawing: 0..1;
        _allowsCutCopyPaste: 0..1;
        _editable: 0..1;
      end;
    __target: id;
    __action: SEL;
    
  public
    class function alloc: NSImageView; message 'alloc';

    function image: NSImage; message 'image';
    procedure setImage(newImage: NSImage); message 'setImage:';
    function imageAlignment: NSImageAlignment; message 'imageAlignment';
    procedure setImageAlignment(newAlign: NSImageAlignment); message 'setImageAlignment:';
    function imageScaling: NSImageScaling; message 'imageScaling';
    procedure setImageScaling(newScaling: NSImageScaling); message 'setImageScaling:';
    function imageFrameStyle: NSImageFrameStyle; message 'imageFrameStyle';
    procedure setImageFrameStyle(newStyle: NSImageFrameStyle); message 'setImageFrameStyle:';
    procedure setEditable(yn: Boolean); message 'setEditable:';
    function isEditable: Boolean; message 'isEditable';
    procedure setAnimates(flag: Boolean); message 'setAnimates:';
    function animates: Boolean; message 'animates';
    function allowsCutCopyPaste: Boolean; message 'allowsCutCopyPaste';
    procedure setAllowsCutCopyPaste(allow: Boolean); message 'setAllowsCutCopyPaste:';
  end; external;

{$endif}
{$endif}
