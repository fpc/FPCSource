{ Parsed from AppKit.framework NSBox.h }


{$ifdef TYPES}
type
  NSBoxPtr = ^NSBox;
{$endif}

{$ifdef TYPES}
type
  NSTitlePosition = NSUInteger;
  NSTitlePositionPtr = ^NSTitlePosition;

const
  NSNoTitle = 0;
  NSAboveTop = 1;
  NSAtTop = 2;
  NSBelowTop = 3;
  NSAboveBottom = 4;
  NSAtBottom = 5;
  NSBelowBottom = 6;

type
  NSBoxType = NSUInteger;
  NSBoxTypePtr = ^NSBoxType;

const
  NSBoxPrimary = 0;
  NSBoxSecondary = 1;
  NSBoxSeparator = 2;
  NSBoxOldStyle = 3;
  NSBoxCustom = 4 { available in 10_5 };
{$endif}

{$ifdef CLASSES}

type
  NSBox = objcclass external (NSView)
  private
    _titleCell: id;
    _contentView: id;
    _offsets: NSSize;
    _borderRect: NSRect;
    _titleRect: NSRect;
    __bFlags: bitpacked record
      case byte of
        0: (_anonBitField___bFlags0: cuint);
        1: (
          borderType: 0..((1 shl 2)-1);
          titlePosition: 0..((1 shl 3)-1);
          backgroundTransparent: 0..1;
          orientation: 0..((1 shl 2)-1);
          needsTile: 0..1;
          transparent: 0..1;
          colorAltInterpretation: 0..1;
          boxType: 0..((1 shl 3)-1);
          useSuperAddSubview: 0..1;
          _RESERVED: 0..((1 shl 17)-1);
        );
      end;
    _unused: id;
  public
    procedure setBorderType(newValue: NSBorderType); message 'setBorderType:';
    function borderType: NSBorderType; message 'borderType';
    procedure setTitlePosition(newValue: NSTitlePosition); message 'setTitlePosition:';
    function titlePosition: NSTitlePosition; message 'titlePosition';
    procedure setBoxType(newValue: NSBoxType); message 'setBoxType:';
    function boxType: NSBoxType; message 'boxType';
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setTitleFont(newValue: NSFont); message 'setTitleFont:';
    function titleFont: NSFont; message 'titleFont';
    function borderRect: NSRect; message 'borderRect';
    function titleRect: NSRect; message 'titleRect';
    function titleCell: id; message 'titleCell';
    procedure sizeToFit; message 'sizeToFit';
    procedure setContentViewMargins(newValue: NSSize); message 'setContentViewMargins:';
    function contentViewMargins: NSSize; message 'contentViewMargins';
    procedure setFrameFromContentFrame (contentFrame: NSRect); message 'setFrameFromContentFrame:';
    procedure setContentView(newValue: id); message 'setContentView:';
    function contentView: id; message 'contentView';
    procedure setTransparent(newValue: ObjCBOOL); message 'setTransparent:';
    function isTransparent: ObjCBOOL; message 'isTransparent';
    procedure setBorderWidth(newValue: CGFloat); message 'setBorderWidth:';
    function borderWidth: CGFloat; message 'borderWidth';
    procedure setCornerRadius(newValue: CGFloat); message 'setCornerRadius:';
    function cornerRadius: CGFloat; message 'cornerRadius';
    procedure setBorderColor(newValue: NSColor); message 'setBorderColor:';
    function borderColor: NSColor; message 'borderColor';
    procedure setFillColor(newValue: NSColor); message 'setFillColor:';
    function fillColor: NSColor; message 'fillColor';
  end;


type
  NSBox_NSDeprecated = objccategory external name 'NSDeprecated' (NSBox)
    procedure setTitleWithMnemonic (stringWithAmpersand: NSString); message 'setTitleWithMnemonic:'; deprecated 'in 10_0, 10_8';
  end;
{$endif}

