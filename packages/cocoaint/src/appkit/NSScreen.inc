{ Parsed from AppKit.framework NSScreen.h }


{$ifdef TYPES}
type
  NSScreenPtr = ^NSScreen;
{$endif}

{$ifdef TYPES}
type
  NSScreenAuxiliaryOpaque = OpaqueType;
  NSScreenAuxiliaryOpaquePtr = ^NSScreenAuxiliaryOpaque;
{$endif}

{$ifdef CLASSES}

type
  NSScreen = objcclass external (NSObject)
  private
    _frame: NSRect;
    _depth: NSWindowDepth;
    _screenNumber: cint;
    _auxiliaryStorage: id;
  public
    class function screens: NSArray; message 'screens';
    class function mainScreen: NSScreen; message 'mainScreen';
    class function deepestScreen: NSScreen; message 'deepestScreen';
    class function screensHaveSeparateSpaces: ObjCBOOL; message 'screensHaveSeparateSpaces'; { available in 10_9 }
    function depth: NSWindowDepth; message 'depth';
    function frame: NSRect; message 'frame';
    function visibleFrame: NSRect; message 'visibleFrame';
    function deviceDescription: NSDictionary; message 'deviceDescription';
    function colorSpace: NSColorSpace; message 'colorSpace';
    function supportedWindowDepths: NSWindowDepthPtr; message 'supportedWindowDepths';
    function convertRectToBacking (aRect: NSRect): NSRect; message 'convertRectToBacking:'; { available in 10_7 }
    function convertRectFromBacking (aRect: NSRect): NSRect; message 'convertRectFromBacking:'; { available in 10_7 }
    function backingAlignedRect_options (aRect: NSRect; options: NSAlignmentOptions): NSRect; message 'backingAlignedRect:options:'; { available in 10_7 }
    function backingScaleFactor: CGFloat; message 'backingScaleFactor';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSScreenColorSpaceDidChangeNotification: NSString { available in 10_6 }; cvar; external;
{$endif}

{$ifdef CLASSES}

type
  NSScreen_NSDeprecated = objccategory external name 'NSDeprecated' (NSScreen)
    function userSpaceScaleFactor: CGFloat; message 'userSpaceScaleFactor'; deprecated 'in 10_4, 10_7, "Use -convertRectToBacking: or -backingScaleFactor instead"';
  end;
{$endif}

