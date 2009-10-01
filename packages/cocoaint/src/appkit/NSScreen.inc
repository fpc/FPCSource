{ Parsed from Appkit.framework NSScreen.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:07 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSSCREEN_PAS_T}
{$define NSSCREEN_PAS_T}

{ Types }
type
  NSScreenAuxiliary = Pointer;
  NSScreenAuxiliaryOpaque = NSScreenAuxiliary;

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

{$endif}
{$endif}

{$ifdef FORWARD}
  NSScreen = objcclass;
  NSScreenPointer = ^NSScreen;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSCREEN_PAS_C}
{$define NSSCREEN_PAS_C}

{ NSScreen }
  NSScreen = objcclass(NSObject)
  private
    __frame: NSRect;
    __depth: NSWindowDepth;
    __screenNumber: cint;
    __auxiliaryStorage: NSScreenAuxiliaryOpaque;
    
  public
    class function alloc: NSScreen; message 'alloc';

    class function screens: NSArray; message 'screens';
    class function mainScreen: NSScreen; message 'mainScreen';
    class function deepestScreen: NSScreen; message 'deepestScreen';
    function depth: NSWindowDepth; message 'depth';
    function frame: NSRect; message 'frame';
    function visibleFrame: NSRect; message 'visibleFrame';
    function deviceDescription: NSDictionary; message 'deviceDescription';
    function supportedWindowDepths: NSWindowDepth; message 'supportedWindowDepths';
    function userSpaceScaleFactor: CGFloat; message 'userSpaceScaleFactor';
  end; external;

{$endif}
{$endif}
