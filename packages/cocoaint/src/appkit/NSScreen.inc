{ Parsed from Appkit.framework NSScreen.h }
{ Version: 2.1.2 - Wed Dec 8 10:06:06 CET 2010 }


{$ifdef TYPES}
{$ifndef NSSCREEN_PAS_T}
{$define NSSCREEN_PAS_T}

{ Types }
type
  NSScreenAuxiliary = record end;
  NSScreenAuxiliaryOpaque = NSScreenAuxiliary;
  NSScreenAuxiliaryOpaquePtr = ^NSScreenAuxiliaryOpaque;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSSCREEN_PAS_R}
{$define NSSCREEN_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSSCREEN_PAS_F}
{$define NSSCREEN_PAS_F}

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSSCREEN_PAS_S}
{$define NSSCREEN_PAS_S}

{ External string constants }
var
  NSScreenColorSpaceDidChangeNotification: NSString; cvar; external;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScreen = objcclass;
  NSScreenPointer = ^NSScreen;
  NSScreenPtr = NSScreenPointer;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCREEN_PAS_C}
{$define NSSCREEN_PAS_C}

{ NSScreen }
  NSScreen = objcclass(NSObject)
  private
    _frame: NSRect;
    _depth: NSWindowDepth;
    _screenNumber: cint;
    _auxiliaryStorage: id;
    
  public
    class function alloc: NSScreen; message 'alloc';

    class function screens: NSArray; message 'screens';
    class function mainScreen: NSScreen; message 'mainScreen';
    class function deepestScreen: NSScreen; message 'deepestScreen';
    function depth: NSWindowDepth; message 'depth';
    function frame: NSRect; message 'frame';
    function visibleFrame: NSRect; message 'visibleFrame';
    function deviceDescription: NSDictionary; message 'deviceDescription';
    function colorSpace: NSColorSpace; message 'colorSpace';
    function supportedWindowDepths: NSWindowDepthPtr; message 'supportedWindowDepths';
    function userSpaceScaleFactor: CGFloat; message 'userSpaceScaleFactor';
  end; external;

{$endif}
{$endif}
