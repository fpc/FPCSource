{ Parsed from Appkit.framework NSGraphics.h }

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
  NSColorRenderingIntentDefault = 0;
  NSColorRenderingIntentAbsoluteColorimetric = 1;
  NSColorRenderingIntentRelativeColorimetric = 2;
  NSColorRenderingIntentPerceptual = 3;
  NSColorRenderingIntentSaturation = 4;

const
  NSWindowDepthTwentyfourBitRGB = $208;
  NSWindowDepthSixtyfourBitRGB = $210;
  NSWindowDepthOnehundredtwentyeightBitRGB = $220;

const
  NSAnimationEffectDisappearingItemDefault = 0;
  NSAnimationEffectPoof = 10;

{ Types }
type
  NSCompositingOperation = NSUInteger;
  NSCompositingOperationPtr = ^NSCompositingOperation;
  NSBackingStoreType = NSUInteger;
  NSBackingStoreTypePtr = ^NSBackingStoreType;
  NSWindowOrderingMode = NSInteger;
  NSWindowOrderingModePtr = ^NSWindowOrderingMode;
  NSFocusRingPlacement = NSUInteger;
  NSFocusRingPlacementPtr = ^NSFocusRingPlacement;
  NSFocusRingType = NSUInteger;
  NSFocusRingTypePtr = ^NSFocusRingType;
  NSColorRenderingIntent = NSInteger;
  NSColorRenderingIntentPtr = ^NSColorRenderingIntent;
  NSWindowDepth = cint;
  NSWindowDepthPtr = ^NSWindowDepth;
  NSAnimationEffect = NSUInteger;
  NSAnimationEffectPtr = ^NSAnimationEffect;

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
function NSBestDepth(colorSpace: NSString; bps: NSInteger; bpp: NSInteger; planar: Boolean; exactMatch: pboolean): NSWindowDepth; cdecl; external;
function NSPlanarFromDepth(depth: NSWindowDepth): Boolean; cdecl; external;
function NSColorSpaceFromDepth(depth: NSWindowDepth): NSString; cdecl; external;
function NSBitsPerSampleFromDepth(depth: NSWindowDepth): NSInteger; cdecl; external;
function NSBitsPerPixelFromDepth(depth: NSWindowDepth): NSInteger; cdecl; external;
function NSNumberOfColorComponents(colorSpaceName: NSString): NSInteger; cdecl; external;
function NSAvailableWindowDepths: NSWindowDepthPtr; cdecl; external;
procedure NSRectFill(aRect: NSRect); cdecl; external;
procedure NSRectFillList(rects: NSRectPtr; count: NSInteger); cdecl; external;
procedure NSRectFillListWithGrays(rects: NSRectPtr; grays: CGFloatPtr; num: NSInteger); cdecl; external;
procedure NSRectFillListWithColors(rects: NSRectPtr; colors: NSColorPointer; num: NSInteger); cdecl; external;
procedure NSRectFillUsingOperation(aRect: NSRect; op: NSCompositingOperation); cdecl; external;
procedure NSRectFillListUsingOperation(rects: NSRectPtr; count: NSInteger; op: NSCompositingOperation); cdecl; external;
procedure NSRectFillListWithColorsUsingOperation(rects: NSRectPtr; colors: NSColorPointer; num: NSInteger; op: NSCompositingOperation); cdecl; external;
procedure NSFrameRect(aRect: NSRect); cdecl; external;
procedure NSFrameRectWithWidth(aRect: NSRect; frameWidth: CGFloat); cdecl; external;
procedure NSFrameRectWithWidthUsingOperation(aRect: NSRect; frameWidth: CGFloat; op: NSCompositingOperation); cdecl; external;
procedure NSRectClip(aRect: NSRect); cdecl; external;
procedure NSRectClipList(rects: NSRectPtr; count: NSInteger); cdecl; external;
function NSDrawTiledRects(boundsRect: NSRect; clipRect: NSRect; sides: NSRectEdgePtr; grays: CGFloatPtr; count: NSInteger): NSRect; cdecl; external;
procedure NSDrawGrayBezel(aRect: NSRect; clipRect: NSRect); cdecl; external;
procedure NSDrawGroove(aRect: NSRect; clipRect: NSRect); cdecl; external;
procedure NSDrawWhiteBezel(aRect: NSRect; clipRect: NSRect); cdecl; external;
procedure NSDrawButton(aRect: NSRect; clipRect: NSRect); cdecl; external;
procedure NSEraseRect(aRect: NSRect); cdecl; external;
function NSReadPixel(passedPoint: NSPoint): NSColor; cdecl; external;
procedure NSDrawBitmap(rect: NSRect; width: NSInteger; height: NSInteger; bps: NSInteger; spp: NSInteger; bpp: NSInteger; bpr: NSInteger; isPlanar: Boolean; hasAlpha: Boolean; colorSpaceName: NSString; data: PPChar {array of PChar}); cdecl; external;
procedure NSCopyBits(srcGState: NSInteger; srcRect: NSRect; destPoint: NSPoint); cdecl; external;
procedure NSHighlightRect(aRect: NSRect); cdecl; external;
procedure NSBeep; cdecl; external;
function NSGetWindowServerMemory(context: NSInteger; virtualMemory: NSIntegerPtr; windowBackingMemory: NSIntegerPtr; windowDumpString: NSStringPointer): NSInteger; cdecl; external;
function NSDrawColorTiledRects(boundsRect: NSRect; clipRect: NSRect; sides: NSRectEdgePtr; colors: NSColorPointer; count: NSInteger): NSRect; cdecl; external;
procedure NSDrawDarkBezel(aRect: NSRect; clipRect: NSRect); cdecl; external;
procedure NSDrawLightBezel(aRect: NSRect; clipRect: NSRect); cdecl; external;
procedure NSDottedFrameRect(aRect: NSRect); cdecl; external;
procedure NSDrawWindowBackground(aRect: NSRect); cdecl; external;
procedure NSSetFocusRingStyle(placement: NSFocusRingPlacement); cdecl; external;
procedure NSDisableScreenUpdates; cdecl; external;
procedure NSEnableScreenUpdates; cdecl; external;
procedure NSShowAnimationEffect(animationEffect: NSAnimationEffect; centerLocation: NSPoint; size: NSSize; animationDelegate: id; didEndSelector: SEL; contextInfo: Pointer); cdecl; external;
procedure NSCountWindows(count: NSIntegerPtr); cdecl; external;
procedure NSWindowList(size: NSInteger; list: NSIntegerPtr {array of NSInteger}); cdecl; external;
procedure NSCountWindowsForContext(context: NSInteger; count: NSIntegerPtr); cdecl; external;
procedure NSWindowListForContext(context: NSInteger; size: NSInteger; list: NSIntegerPtr {array of NSInteger}); cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSGRAPHICS_PAS_S}
{$define NSGRAPHICS_PAS_S}

{ External string constants }
var
  NSCalibratedWhiteColorSpace: NSString; cvar; external;
  NSCalibratedRGBColorSpace: NSString; cvar; external;
  NSDeviceWhiteColorSpace: NSString; cvar; external;
  NSDeviceRGBColorSpace: NSString; cvar; external;
  NSDeviceCMYKColorSpace: NSString; cvar; external;
  NSNamedColorSpace: NSString; cvar; external;
  NSPatternColorSpace: NSString; cvar; external;
  NSCustomColorSpace: NSString; cvar; external;
  NSCalibratedBlackColorSpace: NSString deprecated 'in Mac OS X 10.6 and later'; cvar; external;
  NSDeviceBlackColorSpace: NSString deprecated 'in Mac OS X 10.6 and later'; cvar; external;
  NSDeviceResolution: NSString; cvar; external;
  NSDeviceColorSpaceName: NSString; cvar; external;
  NSDeviceBitsPerSample: NSString; cvar; external;
  NSDeviceIsScreen: NSString; cvar; external;
  NSDeviceIsPrinter: NSString; cvar; external;
  NSDeviceSize: NSString; cvar; external;

{ External symbols }
var
  NSWhite: CGFloat; cvar; external;
  NSLightGray: CGFloat; cvar; external;
  NSDarkGray: CGFloat; cvar; external;
  NSBlack: CGFloat; cvar; external;

{$endif}
{$endif}
