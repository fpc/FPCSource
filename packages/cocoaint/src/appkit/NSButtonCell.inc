{ Parsed from AppKit.framework NSButtonCell.h }


{$ifdef TYPES}
type
  NSButtonCellPtr = ^NSButtonCell;
{$endif}

{$ifdef TYPES}
type
  NSButtonType = NSUInteger;
  NSButtonTypePtr = ^NSButtonType;

const
  NSMomentaryLightButton = 0;
  NSPushOnPushOffButton = 1;
  NSToggleButton = 2;
  NSSwitchButton = 3;
  NSRadioButton = 4;
  NSMomentaryChangeButton = 5;
  NSOnOffButton = 6;
  NSMomentaryPushInButton = 7;
  NSMomentaryPushButton = 0 deprecated 'in 10_0, 10_9';
  NSMomentaryLight = 7 deprecated 'in 10_0, 10_9';

type
  NSBezelStyle = NSUInteger;
  NSBezelStylePtr = ^NSBezelStyle;

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
  NSInlineBezelStyle = 15 { available in 10_7 };
  NSSmallIconButtonBezelStyle = 2 deprecated 'in 10_0, 10_0';

type
  __BCFlags = bitpacked record
    case byte of
      0: (_anonBitField___BCFlags0: cuint);
      1: (
        {$ifdef __BIG_ENDIAN__}
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
        inIntermediateDisclosure: 0..1;
        transparent: 0..1;
        inset: 0..((1 shl 2)-1);
        doesNotDimImage: 0..1;
        suppressAXValueChangeNote: 0..1;
        isDrawingDisclosure: 0..1;
        hasTitleTextField: 0..1;
        useButtonImageSource: 0..1;
        isDrawingFocus: 0..1;
        allowTitleTightening: 0..1;
        __reserved: 0..((1 shl 6)-1);
        {$else}
        __reserved_: 0..((1 shl 6)-1);
        allowTitleTightening_: 0..1;
        isDrawingFocus_: 0..1;
        useButtonImageSource_: 0..1;
        hasTitleTextField_: 0..1;
        isDrawingDisclosure_: 0..1;
        suppressAXValueChangeNote_: 0..1;
        doesNotDimImage_: 0..1;
        inset_: 0..((1 shl 2)-1);
        transparent_: 0..1;
        inIntermediateDisclosure_: 0..1;
        hasKeyEquivalentInsteadOfImage_: 0..1;
        imageSizeDiff_: 0..1;
        imageAndText_: 0..1;
        bottomOrLeft_: 0..1;
        horizontal_: 0..1;
        imageOverlaps_: 0..1;
        bordered_: 0..1;
        drawing_: 0..1;
        lightByGray_: 0..1;
        lightByBackground_: 0..1;
        lightByContents_: 0..1;
        changeGray_: 0..1;
        changeBackground_: 0..1;
        changeContents_: 0..1;
        pushIn_: 0..1;
        {$endif}
      );
    end;
type
  _BCFlags = __BCFlags;
type
  _BCFlagsPtr = ^__BCFlags;
type
  __BCFlagsPtr = ^__BCFlags;
type
  __BCFlags2 = bitpacked record
    case byte of
      0: (_anonBitField___BCFlags20: cuint);
      1: (
        {$ifdef __BIG_ENDIAN__}
        keyEquivalentModifierMask: 0..((1 shl 24)-1);
        imageScaling: 0..((1 shl 2)-1);
        bezelStyle2: 0..1;
        mouseInside: 0..1;
        showsBorderOnlyWhileMouseInside: 0..1;
        bezelStyle: 0..((1 shl 3)-1);
        {$else}
        bezelStyle_: 0..((1 shl 3)-1);
        showsBorderOnlyWhileMouseInside_: 0..1;
        mouseInside_: 0..1;
        bezelStyle2_: 0..1;
        imageScaling_: 0..((1 shl 2)-1);
        keyEquivalentModifierMask_: 0..((1 shl 24)-1);
        {$endif}
      );
    end;
type
  _BCFlags2 = __BCFlags2;
type
  _BCFlags2Ptr = ^__BCFlags2;
type
  __BCFlags2Ptr = ^__BCFlags2;
{$endif}

{$ifdef CLASSES}

type
  NSButtonCell = objcclass external (NSActionCell)
  private
    _altContents: NSString;
    _sound: id;
    _keyEquivalent: NSString;
    _bcFlags2: _BCFlags2;
    _periodicDelay: cushort;
    _periodicInterval: cushort;
    _bcFlags: _BCFlags;
    _normalImage: NSImage;
    _alternateImageOrKeyEquivalentFont: id;
  public
    procedure setTitle(newValue: NSString); message 'setTitle:';
    function title: NSString; message 'title';
    procedure setAlternateTitle(newValue: NSString); message 'setAlternateTitle:';
    function alternateTitle: NSString; message 'alternateTitle';
    procedure setAlternateImage(newValue: NSImage); message 'setAlternateImage:';
    function alternateImage: NSImage; message 'alternateImage';
    procedure setImagePosition(newValue: NSCellImagePosition); message 'setImagePosition:';
    function imagePosition: NSCellImagePosition; message 'imagePosition';
    procedure setImageScaling(newValue: NSImageScaling); message 'setImageScaling:';
    function imageScaling: NSImageScaling; message 'imageScaling';
    procedure setHighlightsBy(newValue: NSCellStyleMask); message 'setHighlightsBy:';
    function highlightsBy: NSCellStyleMask; message 'highlightsBy';
    procedure setShowsStateBy(newValue: NSCellStyleMask); message 'setShowsStateBy:';
    function showsStateBy: NSCellStyleMask; message 'showsStateBy';
    procedure setButtonType (aType: NSButtonType); message 'setButtonType:';
    function isOpaque: ObjCBOOL; message 'isOpaque';
    procedure setTransparent(newValue: ObjCBOOL); message 'setTransparent:';
    function isTransparent: ObjCBOOL; message 'isTransparent';
    procedure setPeriodicDelay_interval (delay: single; interval: single); message 'setPeriodicDelay:interval:';
    procedure getPeriodicDelay_interval (delay: psingle; interval: psingle); message 'getPeriodicDelay:interval:';
    procedure setKeyEquivalent(newValue: NSString); message 'setKeyEquivalent:';
    function keyEquivalent: NSString; message 'keyEquivalent';
    procedure setKeyEquivalentModifierMask(newValue: NSUInteger); message 'setKeyEquivalentModifierMask:';
    function keyEquivalentModifierMask: NSUInteger; message 'keyEquivalentModifierMask';
    procedure setKeyEquivalentFont(newValue: NSFont); message 'setKeyEquivalentFont:';
    function keyEquivalentFont: NSFont; message 'keyEquivalentFont';
    procedure setKeyEquivalentFont_size (fontName: NSString; fontSize: CGFloat); message 'setKeyEquivalentFont:size:';
    procedure performClick (sender: id); message 'performClick:';
    procedure drawImage_withFrame_inView (image_: NSImage; frame: NSRect; controlView_: NSView); message 'drawImage:withFrame:inView:';
    function drawTitle_withFrame_inView (title_: NSAttributedString; frame: NSRect; controlView_: NSView): NSRect; message 'drawTitle:withFrame:inView:';
    procedure drawBezelWithFrame_inView (frame: NSRect; controlView_: NSView); message 'drawBezelWithFrame:inView:';
  end;
{$endif}

{$ifdef TYPES}
type
  NSGradientType = NSUInteger;
  NSGradientTypePtr = ^NSGradientType;

const
  NSGradientNone = 0;
  NSGradientConcaveWeak = 1;
  NSGradientConcaveStrong = 2;
  NSGradientConvexWeak = 3;
  NSGradientConvexStrong = 4;
{$endif}

{$ifdef CLASSES}

type
  NSButtonCellExtensions = objccategory external (NSButtonCell)
    procedure setGradientType(newValue: NSGradientType); message 'setGradientType:';
    function gradientType: NSGradientType; message 'gradientType';
    procedure setImageDimsWhenDisabled(newValue: ObjCBOOL); message 'setImageDimsWhenDisabled:';
    function imageDimsWhenDisabled: ObjCBOOL; message 'imageDimsWhenDisabled';
    procedure setShowsBorderOnlyWhileMouseInside(newValue: ObjCBOOL); message 'setShowsBorderOnlyWhileMouseInside:';
    function showsBorderOnlyWhileMouseInside: ObjCBOOL; message 'showsBorderOnlyWhileMouseInside';
    procedure mouseEntered (event: NSEvent); message 'mouseEntered:';
    procedure mouseExited (event: NSEvent); message 'mouseExited:';
    procedure setBackgroundColor(newValue: NSColor); message 'setBackgroundColor:';
    function backgroundColor: NSColor; message 'backgroundColor';
  end;

type
  NSButtonCellAttributedStringMethods = objccategory external (NSButtonCell)
    procedure setAttributedTitle(newValue: NSAttributedString); message 'setAttributedTitle:';
    function attributedTitle: NSAttributedString; message 'attributedTitle';
    procedure setAttributedAlternateTitle(newValue: NSAttributedString); message 'setAttributedAlternateTitle:';
    function attributedAlternateTitle: NSAttributedString; message 'attributedAlternateTitle';
  end;

type
  NSButtonCellBezelStyles = objccategory external (NSButtonCell)
    procedure setBezelStyle(newValue: NSBezelStyle); message 'setBezelStyle:';
    function bezelStyle: NSBezelStyle; message 'bezelStyle';
  end;

type
  NSButtonCellSoundExtensions = objccategory external (NSButtonCell)
    procedure setSound(newValue: NSSound); message 'setSound:';
    function sound: NSSound; message 'sound';
  end;

type
  NSButtonCell_NSKeyboardUI = objccategory external name 'NSKeyboardUI' (NSButtonCell)
    procedure setTitleWithMnemonic (stringWithAmpersand: NSString); message 'setTitleWithMnemonic:'; deprecated 'in 10_0, 10_8';
    procedure setAlternateTitleWithMnemonic (stringWithAmpersand: NSString); message 'setAlternateTitleWithMnemonic:'; deprecated 'in 10_0, 10_8';
    procedure setAlternateMnemonicLocation (location: NSUInteger); message 'setAlternateMnemonicLocation:'; deprecated 'in 10_0, 10_8';
    function alternateMnemonicLocation: NSUInteger; message 'alternateMnemonicLocation'; deprecated 'in 10_0, 10_8';
    function alternateMnemonic: NSString; message 'alternateMnemonic'; deprecated 'in 10_0, 10_8';
  end;
{$endif}

