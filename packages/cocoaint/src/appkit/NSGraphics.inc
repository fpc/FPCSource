{ Parsed from Appkit.framework NSGraphics.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:47 ICT 2009 }


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
  NSCompositingOperation = NSUInteger;
  NSBackingStoreType = NSUInteger;
  NSWindowOrderingMode = NSInteger;
  NSFocusRingPlacement = NSUInteger;
  NSFocusRingType = NSUInteger;
  NSWindowDepth = cint;
  NSAnimationEffect = NSUInteger;

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
function NSBitsPerSampleFromDepth(depth: NSWindowDepth): NSInteger; cdecl; external name 'NSBitsPerSampleFromDepth';
function NSBitsPerPixelFromDepth(depth: NSWindowDepth): NSInteger; cdecl; external name 'NSBitsPerPixelFromDepth';
function NSNumberOfColorComponents(colorSpaceNamePointer: NSString): NSInteger; cdecl; external name 'NSNumberOfColorComponents';
procedure NSRectFill(aRect: NSRect); cdecl; external name 'NSRectFill';
procedure NSRectFillList(rectsPointer: NSRect; count: NSInteger); cdecl; external name 'NSRectFillList';
procedure NSRectFillListWithGrays(rectsPointer: NSRect; graysPointer: CGFloat; num: NSInteger); cdecl; external name 'NSRectFillListWithGrays';
procedure NSRectFillListWithColors(rectsPointer: NSRect; colors: Pointer {NSColor}; num: NSInteger); cdecl; external name 'NSRectFillListWithColors';
procedure NSRectFillUsingOperation(aRect: NSRect; op: NSCompositingOperation); cdecl; external name 'NSRectFillUsingOperation';
procedure NSRectFillListUsingOperation(rectsPointer: NSRect; count: NSInteger; op: NSCompositingOperation); cdecl; external name 'NSRectFillListUsingOperation';
procedure NSRectFillListWithColorsUsingOperation(rectsPointer: NSRect; colors: Pointer {NSColor}; num: NSInteger; op: NSCompositingOperation); cdecl; external name 'NSRectFillListWithColorsUsingOperation';
procedure NSFrameRect(aRect: NSRect); cdecl; external name 'NSFrameRect';
procedure NSFrameRectWithWidth(aRect: NSRect; frameWidth: CGFloat); cdecl; external name 'NSFrameRectWithWidth';
procedure NSFrameRectWithWidthUsingOperation(aRect: NSRect; frameWidth: CGFloat; op: NSCompositingOperation); cdecl; external name 'NSFrameRectWithWidthUsingOperation';
procedure NSRectClip(aRect: NSRect); cdecl; external name 'NSRectClip';
procedure NSRectClipList(rectsPointer: NSRect; count: NSInteger); cdecl; external name 'NSRectClipList';
function NSDrawTiledRects(boundsRect: NSRect; clipRect: NSRect; sidesPointer: NSRectEdge; graysPointer: CGFloat; count: NSInteger): NSRect; cdecl; external name 'NSDrawTiledRects';
procedure NSDrawGrayBezel(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawGrayBezel';
procedure NSDrawGroove(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawGroove';
procedure NSDrawWhiteBezel(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawWhiteBezel';
procedure NSDrawButton(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawButton';
procedure NSEraseRect(aRect: NSRect); cdecl; external name 'NSEraseRect';
function NSReadPixel(passedPoint: NSPoint): NSColor; cdecl; external name 'NSReadPixel';
procedure NSDrawBitmap(rect: NSRect; width: NSInteger; height: NSInteger; bps: NSInteger; spp: NSInteger; bpp: NSInteger; bpr: NSInteger; isPlanar: Boolean; hasAlpha: Boolean; colorSpaceNamePointer: NSString; char_: cuint); cdecl; external name 'NSDrawBitmap';
procedure NSCopyBits(srcGState: NSInteger; srcRect: NSRect; destPoint: NSPoint); cdecl; external name 'NSCopyBits';
procedure NSHighlightRect(aRect: NSRect); cdecl; external name 'NSHighlightRect';
procedure NSBeep; cdecl; external name 'NSBeep';
procedure NSCountWindows(countPointer: NSInteger); cdecl; external name 'NSCountWindows';
procedure NSWindowList(size: NSInteger; list: Pointer {array of NSInteger}); cdecl; external name 'NSWindowList';
procedure NSCountWindowsForContext(context: NSInteger; countPointer: NSInteger); cdecl; external name 'NSCountWindowsForContext';
procedure NSWindowListForContext(context: NSInteger; size: NSInteger; list: Pointer {array of NSInteger}); cdecl; external name 'NSWindowListForContext';
function NSGetWindowServerMemory(context: NSInteger; virtualMemoryPointer: NSInteger; windowBackingMemoryPointer: NSInteger; windowDumpString: Pointer {NSString}): NSInteger; cdecl; external name 'NSGetWindowServerMemory';
function NSDrawColorTiledRects(boundsRect: NSRect; clipRect: NSRect; sidesPointer: NSRectEdge; colors: Pointer {NSColor}; count: NSInteger): NSRect; cdecl; external name 'NSDrawColorTiledRects';
procedure NSDrawDarkBezel(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawDarkBezel';
procedure NSDrawLightBezel(aRect: NSRect; clipRect: NSRect); cdecl; external name 'NSDrawLightBezel';
procedure NSDottedFrameRect(aRect: NSRect); cdecl; external name 'NSDottedFrameRect';
procedure NSDrawWindowBackground(aRect: NSRect); cdecl; external name 'NSDrawWindowBackground';
procedure NSSetFocusRingStyle(placement: NSFocusRingPlacement); cdecl; external name 'NSSetFocusRingStyle';
procedure NSDisableScreenUpdates; cdecl; external name 'NSDisableScreenUpdates';
procedure NSEnableScreenUpdates; cdecl; external name 'NSEnableScreenUpdates';
procedure NSShowAnimationEffect(animationEffect: NSAnimationEffect; centerLocation: NSPoint; size: NSSize; animationDelegate: id; didEndSelector: SEL; contextInfoPointer: Pointer); cdecl; external name 'NSShowAnimationEffect';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSGRAPHICS_PAS_T}
{$define NSGRAPHICS_PAS_T}

{ External string constants }
var
  NSCalibratedWhiteColorSpace: NSString; external name '_NSCalibratedWhiteColorSpace';
  NSCalibratedBlackColorSpace: NSString; external name '_NSCalibratedBlackColorSpace';
  NSCalibratedRGBColorSpace: NSString; external name '_NSCalibratedRGBColorSpace';
  NSDeviceWhiteColorSpace: NSString; external name '_NSDeviceWhiteColorSpace';
  NSDeviceBlackColorSpace: NSString; external name '_NSDeviceBlackColorSpace';
  NSDeviceRGBColorSpace: NSString; external name '_NSDeviceRGBColorSpace';
  NSDeviceCMYKColorSpace: NSString; external name '_NSDeviceCMYKColorSpace';
  NSNamedColorSpace: NSString; external name '_NSNamedColorSpace';
  NSPatternColorSpace: NSString; external name '_NSPatternColorSpace';
  NSCustomColorSpace: NSString; external name '_NSCustomColorSpace';
  NSDeviceResolution: NSString; external name '_NSDeviceResolution';
  NSDeviceColorSpaceName: NSString; external name '_NSDeviceColorSpaceName';
  NSDeviceBitsPerSample: NSString; external name '_NSDeviceBitsPerSample';
  NSDeviceIsScreen: NSString; external name '_NSDeviceIsScreen';
  NSDeviceIsPrinter: NSString; external name '_NSDeviceIsPrinter';
  NSDeviceSize: NSString; external name '_NSDeviceSize';

{ External symbols }
var
  NSWhite: CGFloat; external name '_NSWhite';
  NSLightGray: CGFloat; external name '_NSLightGray';
  NSDarkGray: CGFloat; external name '_NSDarkGray';
  NSBlack: CGFloat; external name '_NSBlack';

{$endif}
{$endif}
