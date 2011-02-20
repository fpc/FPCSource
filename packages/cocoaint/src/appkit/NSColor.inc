{ Parsed from Appkit.framework NSColor.h }

{$ifdef TYPES}
{$ifndef NSCOLOR_PAS_T}
{$define NSCOLOR_PAS_T}

{ Defines }
const
  NSAppKitVersionNumberWithPatternColorLeakFix = 641.0;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSCOLOR_PAS_R}
{$define NSCOLOR_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSCOLOR_PAS_F}
{$define NSCOLOR_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSCOLOR_PAS_S}
{$define NSCOLOR_PAS_S}

{ External string constants }
var
  NSSystemColorsDidChangeNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSColor = objcclass;
  NSColorPointer = ^NSColor;
  NSColorPtr = NSColorPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSCOLOR_PAS_C}
{$define NSCOLOR_PAS_C}

{ NSColor }
  NSColor = objcclass external (NSObject)
    
  public
    class function colorWithCalibratedWhite_alpha(white: CGFloat; alpha: CGFloat): NSColor; message 'colorWithCalibratedWhite:alpha:';
    class function colorWithCalibratedHue_saturation_brightness_alpha(hue: CGFloat; saturation: CGFloat; brightness: CGFloat; alpha: CGFloat): NSColor; message 'colorWithCalibratedHue:saturation:brightness:alpha:';
    class function colorWithCalibratedRed_green_blue_alpha(red: CGFloat; green: CGFloat; blue: CGFloat; alpha: CGFloat): NSColor; message 'colorWithCalibratedRed:green:blue:alpha:';
    class function colorWithDeviceWhite_alpha(white: CGFloat; alpha: CGFloat): NSColor; message 'colorWithDeviceWhite:alpha:';
    class function colorWithDeviceHue_saturation_brightness_alpha(hue: CGFloat; saturation: CGFloat; brightness: CGFloat; alpha: CGFloat): NSColor; message 'colorWithDeviceHue:saturation:brightness:alpha:';
    class function colorWithDeviceRed_green_blue_alpha(red: CGFloat; green: CGFloat; blue: CGFloat; alpha: CGFloat): NSColor; message 'colorWithDeviceRed:green:blue:alpha:';
    class function colorWithDeviceCyan_magenta_yellow_black_alpha(cyan: CGFloat; magenta: CGFloat; yellow: CGFloat; black: CGFloat; alpha: CGFloat): NSColor; message 'colorWithDeviceCyan:magenta:yellow:black:alpha:';
    class function colorWithCatalogName_colorName(listName: NSString; colorName: NSString): NSColor; message 'colorWithCatalogName:colorName:';
    class function colorWithColorSpace_components_count(space: NSColorSpace; components: CGFloatPtr; numberOfComponents_: NSInteger): NSColor; message 'colorWithColorSpace:components:count:';
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
    function highlightWithLevel(val: CGFloat): NSColor; message 'highlightWithLevel:';
    function shadowWithLevel(val: CGFloat): NSColor; message 'shadowWithLevel:';
    class function colorForControlTint(controlTint: NSControlTint): NSColor; message 'colorForControlTint:';
    class function currentControlTint: NSControlTint; message 'currentControlTint';
    procedure set_; message 'set';
    procedure setFill; message 'setFill';
    procedure setStroke; message 'setStroke';
    function colorSpaceName: NSString; message 'colorSpaceName';
    function colorUsingColorSpaceName(colorSpace_: NSString): NSColor; message 'colorUsingColorSpaceName:';
    function colorUsingColorSpaceName_device(colorSpace_: NSString; deviceDescription: NSDictionary): NSColor; message 'colorUsingColorSpaceName:device:';
    function colorUsingColorSpace(space: NSColorSpace): NSColor; message 'colorUsingColorSpace:';
    function blendedColorWithFraction_ofColor(fraction: CGFloat; color: NSColor): NSColor; message 'blendedColorWithFraction:ofColor:';
    function colorWithAlphaComponent(alpha: CGFloat): NSColor; message 'colorWithAlphaComponent:';
    function catalogNameComponent: NSString; message 'catalogNameComponent';
    function colorNameComponent: NSString; message 'colorNameComponent';
    function localizedCatalogNameComponent: NSString; message 'localizedCatalogNameComponent';
    function localizedColorNameComponent: NSString; message 'localizedColorNameComponent';
    function redComponent: CGFloat; message 'redComponent';
    function greenComponent: CGFloat; message 'greenComponent';
    function blueComponent: CGFloat; message 'blueComponent';
    procedure getRed_green_blue_alpha(red: CGFloatPtr; green: CGFloatPtr; blue: CGFloatPtr; alpha: CGFloatPtr); message 'getRed:green:blue:alpha:';
    function hueComponent: CGFloat; message 'hueComponent';
    function saturationComponent: CGFloat; message 'saturationComponent';
    function brightnessComponent: CGFloat; message 'brightnessComponent';
    procedure getHue_saturation_brightness_alpha(hue: CGFloatPtr; saturation: CGFloatPtr; brightness: CGFloatPtr; alpha: CGFloatPtr); message 'getHue:saturation:brightness:alpha:';
    function whiteComponent: CGFloat; message 'whiteComponent';
    procedure getWhite_alpha(white: CGFloatPtr; alpha: CGFloatPtr); message 'getWhite:alpha:';
    function cyanComponent: CGFloat; message 'cyanComponent';
    function magentaComponent: CGFloat; message 'magentaComponent';
    function yellowComponent: CGFloat; message 'yellowComponent';
    function blackComponent: CGFloat; message 'blackComponent';
    procedure getCyan_magenta_yellow_black_alpha(cyan: CGFloatPtr; magenta: CGFloatPtr; yellow: CGFloatPtr; black: CGFloatPtr; alpha: CGFloatPtr); message 'getCyan:magenta:yellow:black:alpha:';
    function colorSpace: NSColorSpace; message 'colorSpace';
    function numberOfComponents: NSInteger; message 'numberOfComponents';
    procedure getComponents(components: CGFloatPtr); message 'getComponents:';
    function alphaComponent: CGFloat; message 'alphaComponent';
    class function colorFromPasteboard(pasteBoard: NSPasteboard): NSColor; message 'colorFromPasteboard:';
    procedure writeToPasteboard(pasteBoard: NSPasteboard); message 'writeToPasteboard:';
    class function colorWithPatternImage(image: NSImage): NSColor; message 'colorWithPatternImage:';
    function patternImage: NSImage; message 'patternImage';
    procedure drawSwatchInRect(rect: NSRect); message 'drawSwatchInRect:';
    class procedure setIgnoresAlpha(flag: Boolean); message 'setIgnoresAlpha:';
    class function ignoresAlpha: Boolean; message 'ignoresAlpha';
  end;

{ NSQuartzCoreAdditions_NSColorCategory }
  NSQuartzCoreAdditions_NSColorCategory = objccategory external name 'NSQuartzCoreAdditions' (NSColor)
    class function colorWithCIColor(color: CIColor): NSColor; message 'colorWithCIColor:';
  end;

{ NSAppKitAdditionsCategory }
  NSAppKitAdditionsCategory = objccategory external (CIColor)
    function initWithColor(color: NSColor): id; message 'initWithColor:';
  end;

{ NSAppKitColorExtensionsCategory }
  NSAppKitColorExtensionsCategory = objccategory external (NSCoder)
    function decodeNXColor: NSColor; message 'decodeNXColor';
  end;

{$endif}
{$endif}
