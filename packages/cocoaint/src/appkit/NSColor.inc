{ Parsed from AppKit.framework NSColor.h }


{$ifdef TYPES}
type
  NSColorPtr = ^NSColor;
{$endif}

{$ifdef TYPES}
const
  NSAppKitVersionNumberWithPatternColorLeakFix = 641.0;
{$endif}

{$ifdef CLASSES}

type
  NSColor = objcclass external (NSObject, NSCopyingProtocol, NSSecureCodingProtocol, NSPasteboardReadingProtocol, NSPasteboardWritingProtocol)
  public
    function init: instancetype; message 'init'; { NS_DESIGNATED_INITIALIZER }
    function initWithCoder (coder: NSCoder): instancetype; message 'initWithCoder:'; { NS_DESIGNATED_INITIALIZER }
    class function colorWithCalibratedWhite_alpha (white: CGFloat; alpha: CGFloat): NSColor; message 'colorWithCalibratedWhite:alpha:';
    class function colorWithCalibratedHue_saturation_brightness_alpha (hue: CGFloat; saturation: CGFloat; brightness: CGFloat; alpha: CGFloat): NSColor; message 'colorWithCalibratedHue:saturation:brightness:alpha:';
    class function colorWithCalibratedRed_green_blue_alpha (red: CGFloat; green: CGFloat; blue: CGFloat; alpha: CGFloat): NSColor; message 'colorWithCalibratedRed:green:blue:alpha:';
    class function colorWithDeviceWhite_alpha (white: CGFloat; alpha: CGFloat): NSColor; message 'colorWithDeviceWhite:alpha:';
    class function colorWithDeviceHue_saturation_brightness_alpha (hue: CGFloat; saturation: CGFloat; brightness: CGFloat; alpha: CGFloat): NSColor; message 'colorWithDeviceHue:saturation:brightness:alpha:';
    class function colorWithDeviceRed_green_blue_alpha (red: CGFloat; green: CGFloat; blue: CGFloat; alpha: CGFloat): NSColor; message 'colorWithDeviceRed:green:blue:alpha:';
    class function colorWithDeviceCyan_magenta_yellow_black_alpha (cyan: CGFloat; magenta: CGFloat; yellow: CGFloat; black: CGFloat; alpha: CGFloat): NSColor; message 'colorWithDeviceCyan:magenta:yellow:black:alpha:';
    class function colorWithCatalogName_colorName (listName: NSString; colorName: NSString): NSColor; message 'colorWithCatalogName:colorName:';
    class function colorWithColorSpace_components_count (space: NSColorSpace; components: CGFloatPtr; numberOfComponents: NSInteger): NSColor; message 'colorWithColorSpace:components:count:';
    class function colorWithGenericGamma22White_alpha (white: CGFloat; alpha: CGFloat): NSColor; message 'colorWithGenericGamma22White:alpha:'; { available in 10_7 }
    class function colorWithSRGBRed_green_blue_alpha (red: CGFloat; green: CGFloat; blue: CGFloat; alpha: CGFloat): NSColor; message 'colorWithSRGBRed:green:blue:alpha:'; { available in 10_7 }
    class function colorWithWhite_alpha (white: CGFloat; alpha: CGFloat): NSColor; message 'colorWithWhite:alpha:'; { available in 10_9 }
    class function colorWithRed_green_blue_alpha (red: CGFloat; green: CGFloat; blue: CGFloat; alpha: CGFloat): NSColor; message 'colorWithRed:green:blue:alpha:'; { available in 10_9 }
    class function colorWithHue_saturation_brightness_alpha (hue: CGFloat; saturation: CGFloat; brightness: CGFloat; alpha: CGFloat): NSColor; message 'colorWithHue:saturation:brightness:alpha:'; { available in 10_9 }
    class function blackColor: NSColor; message 'blackColor';
    class function darkGrayColor: NSColor; message 'darkGrayColor';
    class function lightGrayColor: NSColor; message 'lightGrayColor';
    class function whiteColor: NSColor; message 'whiteColor';
    class function grayColor: NSColor; message 'grayColor';
    class function redColor: NSColor; message 'redColor';
    class function greenColor: NSColor; message 'greenColor';
    class function blueColor: NSColor; message 'blueColor';
    class function cyanColor: NSColor; message 'cyanColor';
    class function yellowColor: NSColor; message 'yellowColor';
    class function magentaColor: NSColor; message 'magentaColor';
    class function orangeColor: NSColor; message 'orangeColor';
    class function purpleColor: NSColor; message 'purpleColor';
    class function brownColor: NSColor; message 'brownColor';
    class function clearColor: NSColor; message 'clearColor';
    class function controlShadowColor: NSColor; message 'controlShadowColor';
    class function controlDarkShadowColor: NSColor; message 'controlDarkShadowColor';
    class function controlColor: NSColor; message 'controlColor';
    class function controlHighlightColor: NSColor; message 'controlHighlightColor';
    class function controlLightHighlightColor: NSColor; message 'controlLightHighlightColor';
    class function controlTextColor: NSColor; message 'controlTextColor';
    class function controlBackgroundColor: NSColor; message 'controlBackgroundColor';
    class function selectedControlColor: NSColor; message 'selectedControlColor';
    class function secondarySelectedControlColor: NSColor; message 'secondarySelectedControlColor';
    class function selectedControlTextColor: NSColor; message 'selectedControlTextColor';
    class function disabledControlTextColor: NSColor; message 'disabledControlTextColor';
    class function textColor: NSColor; message 'textColor';
    class function textBackgroundColor: NSColor; message 'textBackgroundColor';
    class function selectedTextColor: NSColor; message 'selectedTextColor';
    class function selectedTextBackgroundColor: NSColor; message 'selectedTextBackgroundColor';
    class function gridColor: NSColor; message 'gridColor';
    class function keyboardFocusIndicatorColor: NSColor; message 'keyboardFocusIndicatorColor';
    class function windowBackgroundColor: NSColor; message 'windowBackgroundColor';
    class function underPageBackgroundColor: NSColor; message 'underPageBackgroundColor'; { available in 10_8 }
    class function labelColor: NSColor; message 'labelColor'; { available in 10_10 }
    class function secondaryLabelColor: NSColor; message 'secondaryLabelColor'; { available in 10_10 }
    class function tertiaryLabelColor: NSColor; message 'tertiaryLabelColor'; { available in 10_10 }
    class function quaternaryLabelColor: NSColor; message 'quaternaryLabelColor'; { available in 10_10 }
    class function scrollBarColor: NSColor; message 'scrollBarColor';
    class function knobColor: NSColor; message 'knobColor';
    class function selectedKnobColor: NSColor; message 'selectedKnobColor';
    class function windowFrameColor: NSColor; message 'windowFrameColor';
    class function windowFrameTextColor: NSColor; message 'windowFrameTextColor';
    class function selectedMenuItemColor: NSColor; message 'selectedMenuItemColor';
    class function selectedMenuItemTextColor: NSColor; message 'selectedMenuItemTextColor';
    class function highlightColor: NSColor; message 'highlightColor';
    class function shadowColor: NSColor; message 'shadowColor';
    class function headerColor: NSColor; message 'headerColor';
    class function headerTextColor: NSColor; message 'headerTextColor';
    class function alternateSelectedControlColor: NSColor; message 'alternateSelectedControlColor';
    class function alternateSelectedControlTextColor: NSColor; message 'alternateSelectedControlTextColor';
    class function controlAlternatingRowBackgroundColors: NSArray; message 'controlAlternatingRowBackgroundColors';
    function highlightWithLevel (val: CGFloat): NSColor; message 'highlightWithLevel:';
    function shadowWithLevel (val: CGFloat): NSColor; message 'shadowWithLevel:';
    class function colorForControlTint (controlTint: NSControlTint): NSColor; message 'colorForControlTint:';
    class function currentControlTint: NSControlTint; message 'currentControlTint';
    procedure set_; message 'set';
    procedure setFill; message 'setFill';
    procedure setStroke; message 'setStroke';
    function colorSpaceName: NSString; message 'colorSpaceName';
    function colorUsingColorSpaceName (colorSpace: NSString): NSColor; message 'colorUsingColorSpaceName:';
    function colorUsingColorSpaceName_device (colorSpace: NSString; deviceDescription: NSDictionary): NSColor; message 'colorUsingColorSpaceName:device:';
    function colorUsingColorSpace (space: NSColorSpace): NSColor; message 'colorUsingColorSpace:';
    function blendedColorWithFraction_ofColor (fraction: CGFloat; color: NSColor): NSColor; message 'blendedColorWithFraction:ofColor:';
    function colorWithAlphaComponent (alpha: CGFloat): NSColor; message 'colorWithAlphaComponent:';
    function catalogNameComponent: NSString; message 'catalogNameComponent';
    function colorNameComponent: NSString; message 'colorNameComponent';
    function localizedCatalogNameComponent: NSString; message 'localizedCatalogNameComponent';
    function localizedColorNameComponent: NSString; message 'localizedColorNameComponent';
    function redComponent: CGFloat; message 'redComponent';
    function greenComponent: CGFloat; message 'greenComponent';
    function blueComponent: CGFloat; message 'blueComponent';
    procedure getRed_green_blue_alpha (red: CGFloatPtr; green: CGFloatPtr; blue: CGFloatPtr; alpha: CGFloatPtr); message 'getRed:green:blue:alpha:';
    function hueComponent: CGFloat; message 'hueComponent';
    function saturationComponent: CGFloat; message 'saturationComponent';
    function brightnessComponent: CGFloat; message 'brightnessComponent';
    procedure getHue_saturation_brightness_alpha (hue: CGFloatPtr; saturation: CGFloatPtr; brightness: CGFloatPtr; alpha: CGFloatPtr); message 'getHue:saturation:brightness:alpha:';
    function whiteComponent: CGFloat; message 'whiteComponent';
    procedure getWhite_alpha (white: CGFloatPtr; alpha: CGFloatPtr); message 'getWhite:alpha:';
    function cyanComponent: CGFloat; message 'cyanComponent';
    function magentaComponent: CGFloat; message 'magentaComponent';
    function yellowComponent: CGFloat; message 'yellowComponent';
    function blackComponent: CGFloat; message 'blackComponent';
    procedure getCyan_magenta_yellow_black_alpha (cyan: CGFloatPtr; magenta: CGFloatPtr; yellow: CGFloatPtr; black: CGFloatPtr; alpha: CGFloatPtr); message 'getCyan:magenta:yellow:black:alpha:';
    function colorSpace: NSColorSpace; message 'colorSpace';
    function numberOfComponents: NSInteger; message 'numberOfComponents';
    procedure getComponents (components: CGFloatPtr); message 'getComponents:';
    function alphaComponent: CGFloat; message 'alphaComponent';
    class function colorFromPasteboard (pasteBoard: NSPasteboard): NSColor; message 'colorFromPasteboard:';
    procedure writeToPasteboard (pasteBoard: NSPasteboard); message 'writeToPasteboard:';
    class function colorWithPatternImage (image: NSImage): NSColor; message 'colorWithPatternImage:';
    function patternImage: NSImage; message 'patternImage';
    procedure drawSwatchInRect (rect: NSRect); message 'drawSwatchInRect:';
    class function colorWithCGColor (cgColor: CGColorRef): NSColor; message 'colorWithCGColor:'; { available in 10_8 }
    function CGColor: CGColorRef; message 'CGColor';
    class procedure setIgnoresAlpha (flag: ObjCBOOL); message 'setIgnoresAlpha:';
    class function ignoresAlpha: ObjCBOOL; message 'ignoresAlpha';

    { Adopted protocols }
    function copyWithZone (zone: NSZonePtr): id; message 'copyWithZone:';
    procedure encodeWithCoder (aCoder: NSCoder); message 'encodeWithCoder:';
    function initWithPasteboardPropertyList_ofType (propertyList: id; type_: NSString): id; message 'initWithPasteboardPropertyList:ofType:';
    function pasteboardPropertyListForType (type_: NSString): id; message 'pasteboardPropertyListForType:';
    class function readableTypesForPasteboard (pasteboard: NSPasteboard): NSArray; message 'readableTypesForPasteboard:';
    class function readingOptionsForType_pasteboard (type_: NSString; pasteboard: NSPasteboard): NSPasteboardReadingOptions; message 'readingOptionsForType:pasteboard:';
    class function supportsSecureCoding: ObjCBOOL; message 'supportsSecureCoding';
    function writableTypesForPasteboard (pasteboard: NSPasteboard): NSArray; message 'writableTypesForPasteboard:';
    function writingOptionsForType_pasteboard (type_: NSString; pasteboard: NSPasteboard): NSPasteboardWritingOptions; message 'writingOptionsForType:pasteboard:';
  end;


type
  NSQuartzCoreAdditions = objccategory external (NSColor)
    class function colorWithCIColor (color: CIColor): NSColor; message 'colorWithCIColor:';
  end;

type
  CIColor_NSAppKitAdditions = objccategory external name 'NSAppKitAdditions' (CIColor)
    function initWithColor (color: NSColor): instancetype; message 'initWithColor:';
  end;

type
  NSAppKitColorExtensions = objccategory external (NSCoder)
    function decodeNXColor: NSColor; message 'decodeNXColor'; deprecated 'in 10_0, 10_9';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSSystemColorsDidChangeNotification: NSString; cvar; external;
{$endif}

