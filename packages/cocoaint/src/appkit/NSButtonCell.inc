{ Parsed from Appkit.framework NSButtonCell.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }

{$ifdef HEADER}
{$ifndef NSBUTTONCELL_PAS_H}
{$define NSBUTTONCELL_PAS_H}
type
  NSButtonCellPointer = Pointer;

{$endif}
{$endif}

{$ifdef TYPES}
{$ifndef NSBUTTONCELL_PAS_T}
{$define NSBUTTONCELL_PAS_T}

{ Constants }

const
  NSPushOnPushOffButton = 1;
  NSToggleButton = 2;
  NSSwitchButton = 3;
  NSRadioButton = 4;
  NSMomentaryChangeButton = 5;
  NSOnOffButton = 6;
  NSMomentaryPushButton = 0;
  NSMomentaryLight = 7;

const
  NSRoundedBezelStyle = 1;
  NSRegularSquareBezelStyle = 2;
  NSThickSquareBezelStyle = 3;
  NSThickerSquareBezelStyle = 4;
  NSDisclosureBezelStyle = 5;
  NSShadowlessSquareBezelStyle = 6;
  NSCircularBezelStyle = 7;
  NSTexturedSquareBezelStyle = 8;
  NSHelpButtonBezelStyle = 9;
  NSSmallSquareBezelStyle = 10;
  NSTexturedRoundedBezelStyle = 11;
  NSRoundRectBezelStyle = 12;
  NSRecessedBezelStyle = 13;
  NSRoundedDisclosureBezelStyle = 14;
  NSSmallIconButtonBezelStyle = 2;

const
  NSGradientNone = 0;
  NSGradientConcaveWeak = 1;
  NSGradientConcaveStrong = 2;
  NSGradientConvexWeak = 3;
  NSGradientConvexStrong = 4;

{ Types }
type
  NSButtonType = NSUInteger;
  NSBezelStyle = NSUInteger;
  NSGradientType = NSUInteger;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSBUTTONCELL_PAS_R}
{$define NSBUTTONCELL_PAS_R}

{ Records }
type
  __BCFlags = bitpacked record
{$ifdef fpc_big_endian}
    pushIn: 0..1;
    changeContents: 0..1;
    changeBackground: 0..1;
    changeGray: 0..1;
    lightByContents: 0..1;
    lightByBackground: 0..1;
    lightByGray: 0..1;
    drawing: 0..1;
    bordered: 0..1;
    imageOverlaps: 0..1;
    horizontal: 0..1;
    bottomOrLeft: 0..1;
    imageAndText: 0..1;
    imageSizeDiff: 0..1;
    hasKeyEquivalentInsteadOfImage: 0..1;
    lastState: 0..1;
    transparent: 0..1;
    inset: 0..((1 shl 2)-1);
    doesNotDimImage: 0..1;
    gradientType: 0..((1 shl 3)-1);
    useButtonImageSource: 0..1;
    alternateMnemonicLocation: 0..((1 shl 8)-1);
{$else}
    alternateMnemonicLocation: 0..((1 shl 8)-1);
    useButtonImageSource: 0..1;
    gradientType: 0..((1 shl 3)-1);
    doesNotDimImage: 0..1;
    inset: 0..((1 shl 2)-1);
    transparent: 0..1;
    lastState: 0..1;
    hasKeyEquivalentInsteadOfImage: 0..1;
    imageSizeDiff: 0..1;
    imageAndText: 0..1;
    bottomOrLeft: 0..1;
    horizontal: 0..1;
    imageOverlaps: 0..1;
    bordered: 0..1;
    drawing: 0..1;
    lightByGray: 0..1;
    lightByBackground: 0..1;
    lightByContents: 0..1;
    changeGray: 0..1;
    changeBackground: 0..1;
    changeContents: 0..1;
    pushIn: 0..1;
{$endif}
  end;
_BCFlags = __BCFlags;

type
  __BCFlags2 = bitpacked record
{$ifdef fpc_big_endian}
    keyEquivalentModifierMask: 0..((1 shl 24)-1);
    imageScaling: 0..((1 shl 2)-1);
    bezelStyle2: 0..1;
    mouseInside: 0..1;
    showsBorderOnlyWhileMouseInside: 0..1;
    bezelStyle: 0..((1 shl 3)-1);
{$else}
    bezelStyle: 0..((1 shl 3)-1);
    showsBorderOnlyWhileMouseInside: 0..1;
    mouseInside: 0..1;
    bezelStyle2: 0..1;
    imageScaling: 0..((1 shl 2)-1);
    keyEquivalentModifierMask: 0..((1 shl 24)-1);
{$endif}
  end;
_BCFlags2 = __BCFlags2;


{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSBUTTONCELL_PAS_F}
{$define NSBUTTONCELL_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSBUTTONCELL_PAS_T}
{$define NSBUTTONCELL_PAS_T}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSButtonCell = objcclass;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBUTTONCELL_PAS_C}
{$define NSBUTTONCELL_PAS_C}

{ NSButtonCell }
  NSButtonCell = objcclass(NSActionCell)
  private
    __altContents: NSString;
    __sound: id;
    __keyEquivalent: NSString;
    __bcFlags2: _BCFlags2;
    __periodicDelay: cushort;
    __periodicInterval: cushort;
    __bcFlags: _BCFlags;
    __normalImage: NSImage;
    __alternateImageOrKeyEquivalentFont: id;
    
  public
    class function alloc: NSButtonCell; message 'alloc';

    function title: NSString; message 'title';
    procedure setTitle(aString: NSString); message 'setTitle:';
    function alternateTitle: NSString; message 'alternateTitle';
    procedure setAlternateTitle(aString: NSString); message 'setAlternateTitle:';
    function alternateImage: NSImage; message 'alternateImage';
    procedure setAlternateImage(image_: NSImage); message 'setAlternateImage:';
    function imagePosition: NSCellImagePosition; message 'imagePosition';
    procedure setImagePosition(aPosition: NSCellImagePosition); message 'setImagePosition:';
    function imageScaling: NSImageScaling; message 'imageScaling';
    procedure setImageScaling(scaling: NSImageScaling); message 'setImageScaling:';
    function highlightsBy: NSInteger; message 'highlightsBy';
    procedure setHighlightsBy(aType: NSInteger); message 'setHighlightsBy:';
    function showsStateBy: NSInteger; message 'showsStateBy';
    procedure setShowsStateBy(aType: NSInteger); message 'setShowsStateBy:';
    procedure setButtonType(aType: NSButtonType); message 'setButtonType:';
    function isOpaque: Boolean; message 'isOpaque';
    procedure setFont(fontObj: NSFont); message 'setFont:';
    function isTransparent: Boolean; message 'isTransparent';
    procedure setTransparent(flag: Boolean); message 'setTransparent:';
    procedure setPeriodicDelay_interval(delay: single; interval: single); message 'setPeriodicDelay:interval:';
    procedure getPeriodicDelay_interval(delay: psingle; interval: psingle); message 'getPeriodicDelay:interval:';
    function keyEquivalent: NSString; message 'keyEquivalent';
    procedure setKeyEquivalent(aKeyEquivalent: NSString); message 'setKeyEquivalent:';
    function keyEquivalentModifierMask: NSUInteger; message 'keyEquivalentModifierMask';
    procedure setKeyEquivalentModifierMask(mask: NSUInteger); message 'setKeyEquivalentModifierMask:';
    function keyEquivalentFont: NSFont; message 'keyEquivalentFont';
    procedure setKeyEquivalentFont(fontObj: NSFont); message 'setKeyEquivalentFont:';
    procedure setKeyEquivalentFont_size(fontName: NSString; fontSize: CGFloat); message 'setKeyEquivalentFont:size:';
    procedure performClick(sender: id); message 'performClick:';
    procedure drawImage_withFrame_inView(image_: NSImage; frame: NSRect; controlView_: NSView); message 'drawImage:withFrame:inView:';
    function drawTitle_withFrame_inView(title_: NSAttributedString; frame: NSRect; controlView_: NSView): NSRect; message 'drawTitle:withFrame:inView:';
    procedure drawBezelWithFrame_inView(frame: NSRect; controlView_: NSView); message 'drawBezelWithFrame:inView:';

    { Category: NSKeyboardUI }
    procedure setTitleWithMnemonic(stringWithAmpersand: NSString); message 'setTitleWithMnemonic:';
    procedure setAlternateTitleWithMnemonic(stringWithAmpersand: NSString); message 'setAlternateTitleWithMnemonic:';
    procedure setAlternateMnemonicLocation(location: NSUInteger); message 'setAlternateMnemonicLocation:';
    function alternateMnemonicLocation: NSUInteger; message 'alternateMnemonicLocation';
    function alternateMnemonic: NSString; message 'alternateMnemonic';

    { Category: NSButtonCellExtensions }
    function gradientType: NSGradientType; message 'gradientType';
    procedure setGradientType(type__: NSGradientType); message 'setGradientType:';
    procedure setImageDimsWhenDisabled(flag: Boolean); message 'setImageDimsWhenDisabled:';
    function imageDimsWhenDisabled: Boolean; message 'imageDimsWhenDisabled';
    procedure setShowsBorderOnlyWhileMouseInside(show: Boolean); message 'setShowsBorderOnlyWhileMouseInside:';
    function showsBorderOnlyWhileMouseInside: Boolean; message 'showsBorderOnlyWhileMouseInside';
    procedure mouseEntered(event: NSEvent); message 'mouseEntered:';
    procedure mouseExited(event: NSEvent); message 'mouseExited:';
    function backgroundColor: NSColor; message 'backgroundColor';
    procedure setBackgroundColor(color: NSColor); message 'setBackgroundColor:';

    { Category: NSButtonCellAttributedStringMethods }
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    procedure setAttributedTitle(obj: NSAttributedString); message 'setAttributedTitle:';
    function attributedAlternateTitle: NSAttributedString; message 'attributedAlternateTitle';
    procedure setAttributedAlternateTitle(obj: NSAttributedString); message 'setAttributedAlternateTitle:';

    { Category: NSButtonCellBezelStyles }
    procedure setBezelStyle(bezelStyle_: NSBezelStyle); message 'setBezelStyle:';
    function bezelStyle: NSBezelStyle; message 'bezelStyle';

    { Category: NSButtonCellSoundExtensions }
    procedure setSound(aSound: NSSound); message 'setSound:';
    function sound: NSSound; message 'sound';
  end; external;

{$endif}
{$endif}
