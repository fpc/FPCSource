{ Parsed from AppKit.framework NSImageView.h }


{$ifdef TYPES}
type
  NSImageViewPtr = ^NSImageView;
{$endif}

{$ifdef CLASSES}

type
  NSImageView = objcclass external (NSControl, NSAccessibilityImageProtocol)
  private
    _ivFlags: bitpacked record
      case byte of
        0: (_anonBitField__ivFlags0: cuint);
        1: (
          _hasImageView: 0..1;
          _unused: 0..((1 shl 25)-1);
          _rejectsMultiFileDrops: 0..1;
          _compatibleScalingAndAlignment: 0..1;
          _reserved: 0..1;
          _overridesDrawing: 0..1;
          _allowsCutCopyPaste: 0..1;
          _editable: 0..1;
        );
      end;
    _target: id;
    _action: SEL;
  public
    procedure setImage(newValue: NSImage); message 'setImage:';
    function image: NSImage; message 'image';
    procedure setImageAlignment(newValue: NSImageAlignment); message 'setImageAlignment:';
    function imageAlignment: NSImageAlignment; message 'imageAlignment';
    procedure setImageScaling(newValue: NSImageScaling); message 'setImageScaling:';
    function imageScaling: NSImageScaling; message 'imageScaling';
    procedure setImageFrameStyle(newValue: NSImageFrameStyle); message 'setImageFrameStyle:';
    function imageFrameStyle: NSImageFrameStyle; message 'imageFrameStyle';
    procedure setEditable(newValue: ObjCBOOL); message 'setEditable:';
    function isEditable: ObjCBOOL; message 'isEditable';
    procedure setAnimates(newValue: ObjCBOOL); message 'setAnimates:';
    function animates: ObjCBOOL; message 'animates';
    procedure setAllowsCutCopyPaste(newValue: ObjCBOOL); message 'setAllowsCutCopyPaste:';
    function allowsCutCopyPaste: ObjCBOOL; message 'allowsCutCopyPaste';
  end;
{$endif}

