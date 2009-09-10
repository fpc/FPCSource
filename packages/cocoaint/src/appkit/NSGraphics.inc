{ Parsed from Appkit.framework NSGraphics.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:01 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSGRAPHICS_PAS_T}
{$define NSGRAPHICS_PAS_T}

{ Constants }

const
  NSCompositeClear = 0;
  NSCompositeCopy = 1;
  NSCompositeSourceOver = 2;
  NSCompositeSourceIn = 3;
  NSCompositeSourceOut = 4;
  NSCompositeSourceAtop = 5;
  NSCompositeDestinationOver = 6;
  NSCompositeDestinationIn = 7;
  NSCompositeDestinationOut = 8;
  NSCompositeDestinationAtop = 9;
  NSCompositeXOR = 10;
  NSCompositePlusDarker = 11;
  NSCompositeHighlight = 12;
  NSCompositePlusLighter = 13;

const
  NSBackingStoreRetained = 0;
  NSBackingStoreNonretained = 1;
  NSBackingStoreBuffered = 2;

const
  NSWindowAbove = 1;
  NSWindowBelow = -1;
  NSWindowOut = 0;

const
  NSFocusRingOnly = 0;
  NSFocusRingBelow = 1;
  NSFocusRingAbove = 2;

const
  NSFocusRingTypeDefault = 0;
  NSFocusRingTypeNone = 1;
  NSFocusRingTypeExterior = 2;

const
  NSAnimationEffectDisappearingItemDefault = 0;
  NSAnimationEffectPoof = 10;

{ Types }
type
  NSCompositingOperation = culong;
  NSBackingStoreType = culong;
  NSWindowOrderingMode = clong;
  NSFocusRingPlacement = culong;
  NSFocusRingType = culong;
  NSWindowDepth = cint;
  NSAnimationEffect = culong;

{ CFString constants }
var
  NSCalibratedWhiteColorSpace: CFStringRef; external name '_NSCalibratedWhiteColorSpace';
  NSCalibratedBlackColorSpace: CFStringRef; external name '_NSCalibratedBlackColorSpace';
  NSCalibratedRGBColorSpace: CFStringRef; external name '_NSCalibratedRGBColorSpace';
  NSDeviceWhiteColorSpace: CFStringRef; external name '_NSDeviceWhiteColorSpace';
  NSDeviceBlackColorSpace: CFStringRef; external name '_NSDeviceBlackColorSpace';
  NSDeviceRGBColorSpace: CFStringRef; external name '_NSDeviceRGBColorSpace';
  NSDeviceCMYKColorSpace: CFStringRef; external name '_NSDeviceCMYKColorSpace';
  NSNamedColorSpace: CFStringRef; external name '_NSNamedColorSpace';
  NSPatternColorSpace: CFStringRef; external name '_NSPatternColorSpace';
  NSCustomColorSpace: CFStringRef; external name '_NSCustomColorSpace';
  NSDeviceResolution: CFStringRef; external name '_NSDeviceResolution';
  NSDeviceColorSpaceName: CFStringRef; external name '_NSDeviceColorSpaceName';
  NSDeviceBitsPerSample: CFStringRef; external name '_NSDeviceBitsPerSample';
  NSDeviceIsScreen: CFStringRef; external name '_NSDeviceIsScreen';
  NSDeviceIsPrinter: CFStringRef; external name '_NSDeviceIsPrinter';
  NSDeviceSize: CFStringRef; external name '_NSDeviceSize';

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSGRAPHICS_PAS_R}
{$define NSGRAPHICS_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSGRAPHICS_PAS_F}
{$define NSGRAPHICS_PAS_F}

{ Functions }
function NSBitsPerSampleFromDepth(depth: NSWindowDepth): clong; cdecl; external name 'NSBitsPerSampleFromDepth';
function NSBitsPerPixelFromDepth(depth: NSWindowDepth): clong; cdecl; external name 'NSBitsPerPixelFromDepth';
function NSNumberOfColorComponents(var colorSpaceName: NSString): clong; cdecl; external name 'NSNumberOfColorComponents';
procedure NSRectFill(aRect: NSRect); cdecl; external name 'NSRectFill';
procedure NSRectFillList(var rects: NSRect; count: clong); cdecl; external name 'NSRectFillList';
procedure NSRectFillListWithGrays(var rects: NSRect; var grays: CGFloat; num: clong); cdecl; external name 'NSRectFillListWithGrays';
procedure NSRectFillListWithColors(var rects: NSRect; colors: Pointer {NSColor}; num: clong); cdecl; external name 'NSRectFillListWithColors';
procedure NSRectFillUsingOperation(aRect: NSRect; op: NSCompositingOperation); cdecl; external name 'NSRectFillUsingOperation';
procedure NSRectFillListUsingOperation(var rects: NSRect; count: clong; op: NSCompositingOperation); cdecl; external name 'NSRectFillListUsingOperation';
procedure NSRectFillListWithColorsUsingOperation(var rects: NSRect; colors: Pointer {NSColor}; num: clong; op: NSCompositingOperation); cdecl; external name 'NSRectFillListWithColorsUsingOperation';
procedure NSFrameRect(aRect: NSRect); cdecl; external name 'NSFrameRect';
procedure NSFrameRectWithWidth(aRect: NSRect; frameWidth: CGFloat); cdecl; external name 'NSFrameRectWithWidth';
procedure NSFrameRectWithWidthUsingOperation(aRect: NSRect; frameWidth: CGFloat; op: NSCompositingOperation); cdecl; external name 'NSFrameRectWithWidthUsingOperation';
procedure NSRectClip(aRect: NSRect); cdecl; external name 'NSRectClip';
procedure NSRectClipList(var rects: NSRect; count: clong); cdecl; external name 'NSRectClipList';
function NSDrawTiledRects(boundsRect: NSRect; clipRect: NSRect; var sides: NSRectEdge; var grays: CGFloat; count: clong): NSRect; cdecl; external name 'NSDrawTiledRects';
procedure NSDrawGrayBezel(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawGrayBezel';
procedure NSDrawGroove(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawGroove';
procedure NSDrawWhiteBezel(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawWhiteBezel';
procedure NSDrawButton(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawButton';
procedure NSEraseRect(aRect: NSRect); cdecl; external name 'NSEraseRect';
function NSReadPixel(passedPoint: NSPoint): NSColor; cdecl; external name 'NSReadPixel';
procedure NSDrawBitmap(rect: NSRect; width: clong; height: clong; bps: clong; spp: clong; bpp: clong; bpr: clong; isPlanar: Boolean; hasAlpha: Boolean; var colorSpaceName: NSString; char_: cuint); cdecl; external name 'NSDrawBitmap';
procedure NSCopyBits(srcGState: clong; srcRect: NSRect; destPoint: NSPoint); cdecl; external name 'NSCopyBits';
procedure NSHighlightRect(aRect: NSRect); cdecl; external name 'NSHighlightRect';
procedure NSBeep; cdecl; external name 'NSBeep';
procedure NSCountWindows(var count: clong); cdecl; external name 'NSCountWindows';
procedure NSWindowList(size: clong; list: Pointer {array of clong}); cdecl; external name 'NSWindowList';
procedure NSCountWindowsForContext(context: clong; var count: clong); cdecl; external name 'NSCountWindowsForContext';
procedure NSWindowListForContext(context: clong; size: clong; list: Pointer {array of clong}); cdecl; external name 'NSWindowListForContext';
function NSGetWindowServerMemory(context: clong; var virtualMemory: clong; var windowBackingMemory: clong; windowDumpString: Pointer {NSString}): clong; cdecl; external name 'NSGetWindowServerMemory';
function NSDrawColorTiledRects(boundsRect: NSRect; clipRect: NSRect; var sides: NSRectEdge; colors: Pointer {NSColor}; count: clong): NSRect; cdecl; external name 'NSDrawColorTiledRects';
procedure NSDrawDarkBezel(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawDarkBezel';
procedure NSDrawLightBezel(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawLightBezel';
procedure NSDottedFrameRect(aRect: NSRect); cdecl; external name 'NSDottedFrameRect';
procedure NSDrawWindowBackground(aRect: NSRect); cdecl; external name 'NSDrawWindowBackground';
procedure NSSetFocusRingStyle(placement: NSFocusRingPlacement); cdecl; external name 'NSSetFocusRingStyle';
procedure NSDisableScreenUpdates; cdecl; external name 'NSDisableScreenUpdates';
procedure NSEnableScreenUpdates; cdecl; external name 'NSEnableScreenUpdates';
procedure NSShowAnimationEffect(animationEffect: NSAnimationEffect; centerLocation: NSPoint; size: NSSize; animationDelegate: id; didEndSelector: SEL; var contextInfo: Pointer); cdecl; external name 'NSShowAnimationEffect';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSGRAPHICS_PAS_T}
{$define NSGRAPHICS_PAS_T}

{ External symbols }
var
  NSWhite: CGFloat; external name '_NSWhite';
  NSLightGray: CGFloat; external name '_NSLightGray';
  NSDarkGray: CGFloat; external name '_NSDarkGray';
  NSBlack: CGFloat; external name '_NSBlack';

{$endif}
{$endif}
