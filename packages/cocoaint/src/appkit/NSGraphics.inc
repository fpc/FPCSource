{ Parsed from AppKit.framework NSGraphics.h }

{$ifdef TYPES}
type
  NSCompositingOperation = NSUInteger;
  NSCompositingOperationPtr = ^NSCompositingOperation;

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
  NSCompositeHighlight = 12 deprecated 'in 10_0, 10_0, "Use NSCompositeSourceOver instead"';
  NSCompositePlusLighter = 13;
  NSCompositeMultiply = 14 { available in 10_10 };
  NSCompositeScreen = 15 { available in 10_10 };
  NSCompositeOverlay = 16 { available in 10_10 };
  NSCompositeDarken = 17 { available in 10_10 };
  NSCompositeLighten = 18 { available in 10_10 };
  NSCompositeColorDodge = 19 { available in 10_10 };
  NSCompositeColorBurn = 20 { available in 10_10 };
  NSCompositeSoftLight = 21 { available in 10_10 };
  NSCompositeHardLight = 22 { available in 10_10 };
  NSCompositeDifference = 23 { available in 10_10 };
  NSCompositeExclusion = 24 { available in 10_10 };
  NSCompositeHue = 25 { available in 10_10 };
  NSCompositeSaturation = 26 { available in 10_10 };
  NSCompositeColor = 27 { available in 10_10 };
  NSCompositeLuminosity = 28 { available in 10_10 };

type
  NSBackingStoreType = NSUInteger;
  NSBackingStoreTypePtr = ^NSBackingStoreType;

const
  NSBackingStoreRetained = 0;
  NSBackingStoreNonretained = 1;
  NSBackingStoreBuffered = 2;

type
  NSWindowOrderingMode = NSInteger;
  NSWindowOrderingModePtr = ^NSWindowOrderingMode;

const
  NSWindowAbove = 1;
  NSWindowBelow = -1;
  NSWindowOut = 0;

type
  NSFocusRingPlacement = NSUInteger;
  NSFocusRingPlacementPtr = ^NSFocusRingPlacement;

const
  NSFocusRingOnly = 0;
  NSFocusRingBelow = 1;
  NSFocusRingAbove = 2;

type
  NSFocusRingType = NSUInteger;
  NSFocusRingTypePtr = ^NSFocusRingType;

const
  NSFocusRingTypeDefault = 0;
  NSFocusRingTypeNone = 1;
  NSFocusRingTypeExterior = 2;

type
  NSColorRenderingIntent = NSInteger;
  NSColorRenderingIntentPtr = ^NSColorRenderingIntent;

const
  NSColorRenderingIntentDefault = 0;
  NSColorRenderingIntentAbsoluteColorimetric = 1;
  NSColorRenderingIntentRelativeColorimetric = 2;
  NSColorRenderingIntentPerceptual = 3;
  NSColorRenderingIntentSaturation = 4;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSCalibratedWhiteColorSpace: NSString; cvar; external;
  NSCalibratedRGBColorSpace: NSString; cvar; external;
  NSDeviceWhiteColorSpace: NSString; cvar; external;
  NSDeviceRGBColorSpace: NSString; cvar; external;
  NSDeviceCMYKColorSpace: NSString; cvar; external;
  NSNamedColorSpace: NSString; cvar; external;
  NSPatternColorSpace: NSString; cvar; external;
  NSCustomColorSpace: NSString; cvar; external;
  NSCalibratedBlackColorSpace: NSString deprecated 'in 10_0, 10_6'; cvar; external;
  NSDeviceBlackColorSpace: NSString deprecated 'in 10_0, 10_6'; cvar; external;
{$endif}

{$ifdef TYPES}
const
  NSWindowDepthTwentyfourBitRGB = $208;
  NSWindowDepthSixtyfourBitRGB = $210;
  NSWindowDepthOnehundredtwentyeightBitRGB = $220;

type
  NSWindowDepth = cint;
  NSWindowDepthPtr = ^NSWindowDepth;
{$endif}

{$ifdef FUNCTIONS}
function NSBestDepth(colorSpace: NSString; bps: NSInteger; bpp: NSInteger; planar: ObjCBOOL; exactMatch: pObjCBOOL): NSWindowDepth; cdecl; external;
function NSPlanarFromDepth(depth: NSWindowDepth): ObjCBOOL; cdecl; external;
function NSColorSpaceFromDepth(depth: NSWindowDepth): NSString; cdecl; external;
function NSBitsPerSampleFromDepth(depth: NSWindowDepth): NSInteger; cdecl; external;
function NSBitsPerPixelFromDepth(depth: NSWindowDepth): NSInteger; cdecl; external;
function NSNumberOfColorComponents(colorSpaceName: NSString): NSInteger; cdecl; external;
function NSAvailableWindowDepths: NSWindowDepthPtr; cdecl; external;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  NSWhite: CGFloat; cvar; external;
  NSLightGray: CGFloat; cvar; external;
  NSDarkGray: CGFloat; cvar; external;
  NSBlack: CGFloat; cvar; external;
  NSDeviceResolution: NSString; cvar; external;
  NSDeviceColorSpaceName: NSString; cvar; external;
  NSDeviceBitsPerSample: NSString; cvar; external;
  NSDeviceIsScreen: NSString; cvar; external;
  NSDeviceIsPrinter: NSString; cvar; external;
  NSDeviceSize: NSString; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
procedure NSRectFill(aRect: NSRect); cdecl; external;
procedure NSRectFillList(rects: NSRectPtr; count: NSInteger); cdecl; external;
procedure NSRectFillListWithGrays(rects: NSRectPtr; grays: CGFloatPtr; num: NSInteger); cdecl; external;
procedure NSRectFillListWithColors(rects: NSRectPtr; colors: NSColor; num: NSInteger); cdecl; external;
procedure NSRectFillUsingOperation(aRect: NSRect; op: NSCompositingOperation); cdecl; external;
procedure NSRectFillListUsingOperation(rects: NSRectPtr; count: NSInteger; op: NSCompositingOperation); cdecl; external;
procedure NSRectFillListWithColorsUsingOperation(rects: NSRectPtr; colors: NSColor; num: NSInteger; op: NSCompositingOperation); cdecl; external;
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
procedure NSDrawBitmap(rect: NSRect; width: NSInteger; height: NSInteger; bps: NSInteger; spp: NSInteger; bpp: NSInteger; bpr: NSInteger; isPlanar: ObjCBOOL; hasAlpha: ObjCBOOL; colorSpaceName: NSString; data: PChar { 5 element array of PChar }); cdecl; external;
procedure NSHighlightRect(aRect: NSRect); cdecl; external; deprecated 'in 10_0, 10_0';
procedure NSBeep; cdecl; external;
function NSGetWindowServerMemory(context: NSInteger; virtualMemory: NSIntegerPtr; windowBackingMemory: NSIntegerPtr; windowDumpString: NSStringPtr): NSInteger; cdecl; external;
function NSDrawColorTiledRects(boundsRect: NSRect; clipRect: NSRect; sides: NSRectEdgePtr; colors: NSColorPtr; count: NSInteger): NSRect; cdecl; external;
procedure NSDrawDarkBezel(aRect: NSRect; clipRect: NSRect); cdecl; external;
procedure NSDrawLightBezel(aRect: NSRect; clipRect: NSRect); cdecl; external;
procedure NSDottedFrameRect(aRect: NSRect); cdecl; external;
procedure NSDrawWindowBackground(aRect: NSRect); cdecl; external;
procedure NSSetFocusRingStyle(placement: NSFocusRingPlacement); cdecl; external;
procedure NSDisableScreenUpdates; cdecl; external;
procedure NSEnableScreenUpdates; cdecl; external;
{$endif}

{$ifdef TYPES}
type
  NSAnimationEffect = NSUInteger;
  NSAnimationEffectPtr = ^NSAnimationEffect;

const
  NSAnimationEffectDisappearingItemDefault = 0;
  NSAnimationEffectPoof = 10;
{$endif}

{$ifdef FUNCTIONS}
procedure NSShowAnimationEffect(animationEffect: NSAnimationEffect; centerLocation: NSPoint; size: NSSize; animationDelegate: id; didEndSelector: SEL; contextInfo: pointer); cdecl; external;
procedure NSCountWindows(count: NSIntegerPtr); cdecl; external; deprecated 'in 10_0, 10_6, "Use +[Window windowNumbersWithOptions:] instead"';
procedure NSWindowList(size: NSInteger; list: NSIntegerPtr { variable size array of NSInteger }); cdecl; external; deprecated 'in 10_0, 10_6, "Use +[Window windowNumbersWithOptions:] instead"';
procedure NSCountWindowsForContext(context: NSInteger; count: NSIntegerPtr); cdecl; external; deprecated 'in 10_0, 10_6, "Use +[Window windowNumbersWithOptions:] instead"';
procedure NSWindowListForContext(context: NSInteger; size: NSInteger; list: NSIntegerPtr { variable size array of NSInteger }); cdecl; external; deprecated 'in 10_0, 10_6, "Use +[Window windowNumbersWithOptions:] instead"';
procedure NSCopyBits(srcGState: NSInteger; srcRect: NSRect; destPoint: NSPoint); cdecl; external; deprecated 'in 10_0, 10_10';
{$endif}

