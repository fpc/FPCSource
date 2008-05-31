{ CoreGraphics - CGContext.h
 * Copyright (c) 2000-2003 Apple Computer, Inc.
 * All rights reserved.
 }
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{
    Modified for use with Free Pascal
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit CGContext;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0210}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}

{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
{$elsec}
	{$error Neither __ppc__ nor __i386__ is defined.}
{$endc}
{$setc TARGET_CPU_PPC_64 := FALSE}

{$ifc defined FPC_BIG_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := TRUE}
	{$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := FALSE}
	{$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
	{$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
{$setc ACCESSOR_CALLS_ARE_FUNCTIONS := TRUE}
{$setc CALL_NOT_IN_CARBON := FALSE}
{$setc OLDROUTINENAMES := FALSE}
{$setc OPAQUE_TOOLBOX_STRUCTS := TRUE}
{$setc OPAQUE_UPP_TYPES := TRUE}
{$setc OTCARBONAPPLICATION := TRUE}
{$setc OTKERNEL := FALSE}
{$setc PM_USE_SESSION_APIS := TRUE}
{$setc TARGET_API_MAC_CARBON := TRUE}
{$setc TARGET_API_MAC_OS8 := FALSE}
{$setc TARGET_API_MAC_OSX := TRUE}
{$setc TARGET_CARBON := TRUE}
{$setc TARGET_CPU_68K := FALSE}
{$setc TARGET_CPU_MIPS := FALSE}
{$setc TARGET_CPU_SPARC := FALSE}
{$setc TARGET_OS_MAC := TRUE}
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,CFBase,CGGeometry,CGBase,CFDictionary,CGAffineTransforms,CGColorSpace,CGFont,CGImage,CGPDFDocument,CGPath,CGColor,CGShading,CGPDFPage;
{$ALIGN POWER}


type
	CGContextRef = ^SInt32; { an opaque 32-bit type }


{ Line join styles. }

type
	CGLineJoin = SInt32;
const
	kCGLineJoinMiter = 0;
	kCGLineJoinRound = 1;
	kCGLineJoinBevel = 2;

{ Line cap styles. }

type
	CGLineCap = SInt32;
const
	kCGLineCapButt = 0;
	kCGLineCapRound = 1;
	kCGLineCapSquare = 2;

{ Drawing modes for paths. }

type
	CGPathDrawingMode = SInt32;
const
	kCGPathFill = 0;
	kCGPathEOFill = 1;
	kCGPathStroke = 2;
	kCGPathFillStroke = 3;
	kCGPathEOFillStroke = 4;

{ Drawing modes for text. }

type
	CGTextDrawingMode = SInt32;
const
	kCGTextFill = 0;
	kCGTextStroke = 1;
	kCGTextFillStroke = 2;
	kCGTextInvisible = 3;
	kCGTextFillClip = 4;
	kCGTextStrokeClip = 5;
	kCGTextFillStrokeClip = 6;
	kCGTextClip = 7;

{ Text encodings. }

type
	CGTextEncoding = SInt32;
const
	kCGEncodingFontSpecific = 0;
	kCGEncodingMacRoman = 1;

{ Interpolation quality. }

type
	CGInterpolationQuality = SInt32;
const
	kCGInterpolationDefault = 0;		{ Let the context decide. }
	kCGInterpolationNone = 1;		{ Never interpolate. }
	kCGInterpolationLow = 2;		{ Faster, lower quality. }
	kCGInterpolationHigh = 3;		{ Slower, higher quality. }

{ Blend modes. }

type
	CGBlendMode = SInt32;
const
	kCGBlendModeNormal = 0;
	kCGBlendModeMultiply = 1;
	kCGBlendModeScreen = 2;
	kCGBlendModeOverlay = 3;
	kCGBlendModeDarken = 4;
	kCGBlendModeLighten = 5;
	kCGBlendModeColorDodge = 6;
	kCGBlendModeColorBurn = 7;
	kCGBlendModeSoftLight = 8;
	kCGBlendModeHardLight = 9;
	kCGBlendModeDifference = 10;
	kCGBlendModeExclusion = 11;
	kCGBlendModeHue = 12;
	kCGBlendModeSaturation = 13;
	kCGBlendModeColor = 14;
	kCGBlendModeLuminosity = 15; { Available in Mac OS X 10.4 & later. }


{ Return the CFTypeID for CGContextRefs. }

function CGContextGetTypeID: CFTypeID; external name '_CGContextGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{* Graphics state functions. *}

{ Push a copy of the current graphics state onto the graphics state
 * stack. Note that the path is not considered part of the gstate, and is
 * not saved. }

procedure CGContextSaveGState( c: CGContextRef ); external name '_CGContextSaveGState';

{ Restore the current graphics state from the one on the top of the
 * graphics state stack, popping the graphics state stack in the
 * process. }

procedure CGContextRestoreGState( c: CGContextRef ); external name '_CGContextRestoreGState';

{* Coordinate space transformations. *}

{ Scale the current graphics state's transformation matrix (the CTM) by
 * `(sx, sy)'. }

procedure CGContextScaleCTM( c: CGContextRef; sx: Float32; sy: Float32 ); external name '_CGContextScaleCTM';

{ Translate the current graphics state's transformation matrix (the CTM)
 * by `(tx, ty)'. }

procedure CGContextTranslateCTM( c: CGContextRef; tx: Float32; ty: Float32 ); external name '_CGContextTranslateCTM';

{ Rotate the current graphics state's transformation matrix (the CTM) by
 * `angle' radians. }

procedure CGContextRotateCTM( c: CGContextRef; angle: Float32 ); external name '_CGContextRotateCTM';

{ Concatenate the current graphics state's transformation matrix (the CTM)
 * with the affine transform `transform'. }

procedure CGContextConcatCTM( c: CGContextRef; transform: CGAffineTransform ); external name '_CGContextConcatCTM';

{ Return the current graphics state's transformation matrix. }

function CGContextGetCTM( c: CGContextRef ): CGAffineTransform; external name '_CGContextGetCTM';

{* Drawing attribute functions. *}

{ Set the line width in the current graphics state to `width'. }

procedure CGContextSetLineWidth( c: CGContextRef; width: Float32 ); external name '_CGContextSetLineWidth';

{ Set the line cap in the current graphics state to `cap'. }

procedure CGContextSetLineCap( c: CGContextRef; cap: CGLineCap ); external name '_CGContextSetLineCap';

{ Set the line join in the current graphics state to `join'. }

procedure CGContextSetLineJoin( c: CGContextRef; join: CGLineJoin ); external name '_CGContextSetLineJoin';

{ Set the miter limit in the current graphics state to `limit'. }

procedure CGContextSetMiterLimit( c: CGContextRef; limit: Float32 ); external name '_CGContextSetMiterLimit';

{ Set the line dash patttern in the current graphics state of `c'. }

procedure CGContextSetLineDash( c: CGContextRef; phase: Float32; {const} lengths: {variable-size-array} Float32Ptr; count: size_t ); external name '_CGContextSetLineDash';

{ Set the path flatness parameter in the current graphics state of `c' to
 * `flatness'. }

procedure CGContextSetFlatness( c: CGContextRef; flatness: Float32 ); external name '_CGContextSetFlatness';

{ Set the alpha value in the current graphics state of `c' to `alpha'. }

procedure CGContextSetAlpha( c: CGContextRef; alpha: Float32 ); external name '_CGContextSetAlpha';

{ Set the blend mode of `context' to `mode'. }

procedure CGContextSetBlendMode( context: CGContextRef; mode: CGBlendMode ); external name '_CGContextSetBlendMode'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{* Path construction functions. *}

{ Note that a context has a single path in use at any time: a path is not
 * part of the graphics state. }

{ Begin a new path.  The old path is discarded. }

procedure CGContextBeginPath( c: CGContextRef ); external name '_CGContextBeginPath';

{ Start a new subpath at point `(x, y)' in the context's path. }

procedure CGContextMoveToPoint( c: CGContextRef; x: Float32; y: Float32 ); external name '_CGContextMoveToPoint';

{ Append a straight line segment from the current point to `(x, y)'. }

procedure CGContextAddLineToPoint( c: CGContextRef; x: Float32; y: Float32 ); external name '_CGContextAddLineToPoint';

{ Append a cubic Bezier curve from the current point to `(x,y)', with
 * control points `(cp1x, cp1y)' and `(cp2x, cp2y)'. }

procedure CGContextAddCurveToPoint( c: CGContextRef; cp1x: Float32; cp1y: Float32; cp2x: Float32; cp2y: Float32; x: Float32; y: Float32 ); external name '_CGContextAddCurveToPoint';

{ Append a quadratic curve from the current point to `(x, y)', with
 * control point `(cpx, cpy)'. }

procedure CGContextAddQuadCurveToPoint( c: CGContextRef; cpx: Float32; cpy: Float32; x: Float32; y: Float32 ); external name '_CGContextAddQuadCurveToPoint';

{ Close the current subpath of the context's path. }

procedure CGContextClosePath( c: CGContextRef ); external name '_CGContextClosePath';

{* Path construction convenience functions. *}

{ Add a single rect to the context's path. }

procedure CGContextAddRect( c: CGContextRef; rect: CGRect ); external name '_CGContextAddRect';

{ Add a set of rects to the context's path. }

procedure CGContextAddRects( c: CGContextRef; {const} rects: {variable-size-array} CGRectPtr; count: size_t ); external name '_CGContextAddRects';

{ Add a set of lines to the context's path. }

procedure CGContextAddLines( c: CGContextRef; {const} points: {variable-size-array} CGPointPtr; count: size_t ); external name '_CGContextAddLines';

{ Add an ellipse inside `rect' to the current path of `context'.  See the
 * function `CGPathAddEllipseInRect' for more information on how the path
 * for the ellipse is constructed. }

procedure CGContextAddEllipseInRect( context: CGContextRef; rect: CGRect ); external name '_CGContextAddEllipseInRect'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Add an arc of a circle to the context's path, possibly preceded by a
 * straight line segment.  `(x, y)' is the center of the arc; `radius' is
 * its radius; `startAngle' is the angle to the first endpoint of the arc;
 * `endAngle' is the angle to the second endpoint of the arc; and
 * `clockwise' is 1 if the arc is to be drawn clockwise, 0 otherwise.
 * `startAngle' and `endAngle' are measured in radians. }

procedure CGContextAddArc( c: CGContextRef; x: Float32; y: Float32; radius: Float32; startAngle: Float32; endAngle: Float32; clockwise: SInt32 ); external name '_CGContextAddArc';

{ Add an arc of a circle to the context's path, possibly preceded by a
 * straight line segment.  `radius' is the radius of the arc.  The arc is
 * tangent to the line from the current point to `(x1, y1)', and the line
 * from `(x1, y1)' to `(x2, y2)'. }

procedure CGContextAddArcToPoint( c: CGContextRef; x1: Float32; y1: Float32; x2: Float32; y2: Float32; radius: Float32 ); external name '_CGContextAddArcToPoint';

{ Add `path' to the path of context.  The points in `path' are transformed
 * by the CTM of context before they are added. }

procedure CGContextAddPath( context: CGContextRef; path: CGPathRef ); external name '_CGContextAddPath'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{* Path stroking. *}

{ Replace the path in context with the stroked version of the path, using
 * the parameters of `context' to calculate the stroked path.  The
 * resulting path is created such that filling it with the appropriate
 * color will produce the same results as stroking the original path. You
 * can use this path in the same way you can use the path of any context;
 * for example, you can clip to the stroked version of a path by calling
 * this function followed by a call to "CGContextClipPath". }

procedure CGContextReplacePathWithStrokedPath( c: CGContextRef ); external name '_CGContextReplacePathWithStrokedPath'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{* Path information functions. *}

{ Return true if the context's path contains no elements, false otherwise. }

function CGContextIsPathEmpty( c: CGContextRef ): CBool; external name '_CGContextIsPathEmpty';

{ Return the current point of the current subpath of the context's
 * path. }

function CGContextGetPathCurrentPoint( c: CGContextRef ): CGPoint; external name '_CGContextGetPathCurrentPoint';

{ Return the bounding box of the context's path.  The bounding box is the
 * smallest rectangle completely enclosing all points in the path,
 * including control points for Bezier and quadratic curves. }

function CGContextGetPathBoundingBox( c: CGContextRef ): CGRect; external name '_CGContextGetPathBoundingBox';

{ Return true if `point' is contained in the current path of `context'.  A
 * point is contained within a context's path if it is inside the painted
 * region when the path is stroked or filled with opaque colors using the
 * path drawing mode `mode'.  `point' is specified is user space. }

function CGContextPathContainsPoint( context: CGContextRef; point: CGPoint; mode: CGPathDrawingMode ): CBool; external name '_CGContextPathContainsPoint'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{* Path drawing functions. *}

{ Draw the context's path using drawing mode `mode'. }

procedure CGContextDrawPath( c: CGContextRef; mode: CGPathDrawingMode ); external name '_CGContextDrawPath';

{* Path drawing convenience functions. *}

{ Fill the context's path using the winding-number fill rule.  Any open
 * subpath of the path is implicitly closed. }

procedure CGContextFillPath( c: CGContextRef ); external name '_CGContextFillPath';

{ Fill the context's path using the even-odd fill rule.  Any open subpath
 * of the path is implicitly closed. }

procedure CGContextEOFillPath( c: CGContextRef ); external name '_CGContextEOFillPath';

{ Stroke the context's path. }

procedure CGContextStrokePath( c: CGContextRef ); external name '_CGContextStrokePath';

{ Fill `rect' with the current fill color. }

procedure CGContextFillRect( c: CGContextRef; rect: CGRect ); external name '_CGContextFillRect';

{ Fill `rects', an array of `count' CGRects, with the current fill
 * color. }

procedure CGContextFillRects( c: CGContextRef; {const} rects: {variable-size-array} CGRectPtr; count: size_t ); external name '_CGContextFillRects';

{ Stroke `rect' with the current stroke color and the current linewidth. }

procedure CGContextStrokeRect( c: CGContextRef; rect: CGRect ); external name '_CGContextStrokeRect';

{ Stroke `rect' with the current stroke color, using `width' as the the
 * line width. }

procedure CGContextStrokeRectWithWidth( c: CGContextRef; rect: CGRect; width: Float32 ); external name '_CGContextStrokeRectWithWidth';

{ Clear `rect' (that is, set the region within the rect to
 * transparent). }

procedure CGContextClearRect( c: CGContextRef; rect: CGRect ); external name '_CGContextClearRect';

{ Fill an ellipse (an oval) inside `rect'. }

procedure CGContextFillEllipseInRect( context: CGContextRef; rect: CGRect ); external name '_CGContextFillEllipseInRect'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Stroke an ellipse (an oval) inside `rect'. }

procedure CGContextStrokeEllipseInRect( context: CGContextRef; rect: CGRect ); external name '_CGContextStrokeEllipseInRect'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Stroke a sequence of line segments one after another in `context'.  The
 * line segments are specified by `points', an array of `count' CGPoints.
 * This function is equivalent to
 *   CGContextBeginPath(context);
 *   for (k = 0; k < count; k += 2) begin
 *       CGContextMoveToPoint(context, s[k].x, s[k].y);
 *       CGContextAddLineToPoint(context, s[k+1].x, s[k+1].y);
 *   end;
 *   CGContextStrokePath(context);
 }

procedure CGContextStrokeLineSegments( c: CGContextRef; {const} points: {variable-size-array} CGPointPtr; count: size_t ); external name '_CGContextStrokeLineSegments'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{* Clipping functions. *}

{ Intersect the context's path with the current clip path and use the
 * resulting path as the clip path for subsequent rendering operations.
 * Use the winding-number fill rule for deciding what's inside the path. }

procedure CGContextClip( c: CGContextRef ); external name '_CGContextClip';

{ Intersect the context's path with the current clip path and use the
 * resulting path as the clip path for subsequent rendering operations.
 * Use the even-odd fill rule for deciding what's inside the path. }

procedure CGContextEOClip( c: CGContextRef ); external name '_CGContextEOClip';

{ Add `mask' transformed to `rect' to the clipping area of `context'.  The
 * mask, which may be either an image mask or an image, is mapped into the
 * specified rectangle and intersected with the current clipping area of
 * the context.
 *
 * If `mask' is an image mask, then it clips in a manner identical to the
 * behavior if it were used with "CGContextDrawImage": it indicates an area
 * to be masked out (left unchanged) when drawing.  The source samples of
 * the image mask determine which points of the clipping area are changed,
 * acting as an "inverse alpha": if the value of a source sample in the
 * image mask is S, then the corresponding point in the current clipping
 * area will be multiplied by an alpha of (1-S).  (For example, if S is 1,
 * then the point in the clipping area becomes clear, while if S is 0, the
 * point in the clipping area is unchanged.
 *
 * If `mask' is an image, then it serves as alpha mask and is blended with
 * the current clipping area.  The source samples of mask determine which
 * points of the clipping area are changed: if the value of the source
 * sample in mask is S, then the corresponding point in the current
 * clipping area will be multiplied by an alpha of S.  (For example, if S
 * is 0, then the point in the clipping area becomes clear, while if S is
 * 1, the point in the clipping area is unchanged.
 *
 * If `mask' is an image, then it must be in the DeviceGray color space,
 * may not have alpha, and may not be masked by an image mask or masking
 * color. }

procedure CGContextClipToMask( c: CGContextRef; rect: CGRect; mask: CGImageRef ); external name '_CGContextClipToMask';

{ Return the bounding box of the clip path of `c' in user space.  The
 * bounding box is the smallest rectangle completely enclosing all points
 * in the clip. }

function CGContextGetClipBoundingBox( c: CGContextRef ): CGRect; external name '_CGContextGetClipBoundingBox'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{* Clipping convenience functions. *}

{ Intersect the current clipping path with `rect'.  Note that this
 * function resets the context's path to the empty path. }

procedure CGContextClipToRect( c: CGContextRef; rect: CGRect ); external name '_CGContextClipToRect';

{ Intersect the current clipping path with the clipping region formed by
 * creating a path consisting of all rects in `rects'.  Note that this
 * function resets the context's path to the empty path. }

procedure CGContextClipToRects( c: CGContextRef; {const} rects: {variable-size-array} CGRectPtr; count: size_t ); external name '_CGContextClipToRects';

{* Primitive color functions. *}

{ Set the current fill color in the context `c' to `color'. }

procedure CGContextSetFillColorWithColor( c: CGContextRef; color: CGColorRef ); external name '_CGContextSetFillColorWithColor'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Set the current stroke color in the context `c' to `color'. }

procedure CGContextSetStrokeColorWithColor( c: CGContextRef; color: CGColorRef ); external name '_CGContextSetStrokeColorWithColor'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{* Colorspace functions. *}

{ Set the current fill colorspace in the context `c' to `colorspace'.  As
 * a side-effect, set the fill color to a default value appropriate for the
 * colorspace. }

procedure CGContextSetFillColorSpace( c: CGContextRef; colorspace: CGColorSpaceRef ); external name '_CGContextSetFillColorSpace';

{ Set the current stroke colorspace in the context `c' to `colorspace'.
 * As a side-effect, set the stroke color to a default value appropriate
 * for the colorspace. }

procedure CGContextSetStrokeColorSpace( c: CGContextRef; colorspace: CGColorSpaceRef ); external name '_CGContextSetStrokeColorSpace';

{* Color functions. *}

{ Set the components of the current fill color in the context `c' to the
 * values specifed by `components'.  The number of elements in `components'
 * must be one greater than the number of components in the current fill
 * colorspace (N color components + 1 alpha component).  The current fill
 * colorspace must not be a pattern colorspace. }

procedure CGContextSetFillColor( c: CGContextRef; {const} components: {variable-size-array} Float32Ptr ); external name '_CGContextSetFillColor';

{ Set the components of the current stroke color in the context `c' to the
 * values specifed by `components'.  The number of elements in `components'
 * must be one greater than the number of components in the current stroke
 * colorspace (N color components + 1 alpha component).  The current stroke
 * colorspace must not be a pattern colorspace. }

procedure CGContextSetStrokeColor( c: CGContextRef; {const} components: {variable-size-array} Float32Ptr ); external name '_CGContextSetStrokeColor';

{* Pattern functions. *}

{ Set the components of the current fill color in the context `c' to the
 * values specifed by `components', and set the current fill pattern to
 * `pattern'.  The number of elements in `components' must be one greater
 * than the number of components in the current fill colorspace (N color
 * components + 1 alpha component).  The current fill colorspace must be a
 * pattern colorspace. }

procedure CGContextSetFillPattern( c: CGContextRef; pattern: CGPatternRef; {const} components: {variable-size-array} Float32Ptr ); external name '_CGContextSetFillPattern';

{ Set the components of the current stroke color in the context `c' to the
 * values specifed by `components', and set the current stroke pattern to
 * `pattern'.  The number of elements in `components' must be one greater
 * than the number of components in the current stroke colorspace (N color
 * components + 1 alpha component).  The current stroke colorspace must be
 * a pattern colorspace. }

procedure CGContextSetStrokePattern( c: CGContextRef; pattern: CGPatternRef; {const} components: {variable-size-array} Float32Ptr ); external name '_CGContextSetStrokePattern';

{ Set the pattern phase in the current graphics state of the context `c'
 * to `phase'. }

procedure CGContextSetPatternPhase( c: CGContextRef; phase: CGSize ); external name '_CGContextSetPatternPhase';

{* Color convenience functions. *}

{ Set the current fill colorspace in the context `c' to `DeviceGray' and
 * set the components of the current fill color to `(gray, alpha)'. }

procedure CGContextSetGrayFillColor( c: CGContextRef; gray: Float32; alpha: Float32 ); external name '_CGContextSetGrayFillColor';

{ Set the current stroke colorspace in the context `c' to `DeviceGray' and
 * set the components of the current stroke color to `(gray, alpha)'. }

procedure CGContextSetGrayStrokeColor( c: CGContextRef; gray: Float32; alpha: Float32 ); external name '_CGContextSetGrayStrokeColor';

{ Set the current fill colorspace in the context `c' to `DeviceRGB' and
 * set the components of the current fill color to `(red, green, blue,
 * alpha)'. }

procedure CGContextSetRGBFillColor( c: CGContextRef; red: Float32; green: Float32; blue: Float32; alpha: Float32 ); external name '_CGContextSetRGBFillColor';

{ Set the current stroke colorspace in the context `c' to `DeviceRGB' and
 * set the components of the current stroke color to `(red, green, blue,
 * alpha)'. }

procedure CGContextSetRGBStrokeColor( c: CGContextRef; red: Float32; green: Float32; blue: Float32; alpha: Float32 ); external name '_CGContextSetRGBStrokeColor';

{ Set the current fill colorspace in the context `c' to `DeviceCMYK' and
 * set the components of the current fill color to `(cyan, magenta, yellow,
 * black, alpha)'. }

procedure CGContextSetCMYKFillColor( c: CGContextRef; cyan: Float32; magenta: Float32; yellow: Float32; black: Float32; alpha: Float32 ); external name '_CGContextSetCMYKFillColor';

{ Set the current stroke colorspace in the context `c' to `DeviceCMYK' and
 * set the components of the current stroke color to `(cyan, magenta,
 * yellow, black, alpha)'. }

procedure CGContextSetCMYKStrokeColor( c: CGContextRef; cyan: Float32; magenta: Float32; yellow: Float32; black: Float32; alpha: Float32 ); external name '_CGContextSetCMYKStrokeColor';

{* Rendering intent. *}

{ Set the rendering intent in the current graphics state of context `c' to
 * `intent'. }

procedure CGContextSetRenderingIntent( c: CGContextRef; intent: CGColorRenderingIntent ); external name '_CGContextSetRenderingIntent';

{* Image functions. *}

{ Draw `image' in the rectangular area specified by `rect' in the context
 * `c'.  The image is scaled, if necessary, to fit into `rect'. }

procedure CGContextDrawImage( c: CGContextRef; rect: CGRect; image: CGImageRef ); external name '_CGContextDrawImage';

{ Return the interpolation quality for image rendering of the context `c'.
 * The interpolation quality is a gstate-parameter which controls the level
 * of interpolation performed when an image is interpolated (for example,
 * when scaling the image). Note that it is merely a hint to the context:
 * not all contexts support all interpolation quality levels. }

function CGContextGetInterpolationQuality( c: CGContextRef ): CGInterpolationQuality; external name '_CGContextGetInterpolationQuality';

{ Set the interpolation quality of the context `c' to `quality'. }

procedure CGContextSetInterpolationQuality( c: CGContextRef; quality: CGInterpolationQuality ); external name '_CGContextSetInterpolationQuality';

{* Shadow support. *}

{ Set the shadow parameters in `context'.  `offset' specifies a
 * translation in base-space; `blur' is a non-negative number specifying
 * the amount of blur; `color' specifies the color of the shadow, which may
 * contain a non-opaque alpha value.  If `color' is NULL, it's equivalent
 * to specifying a fully transparent color.  The shadow is a gstate
 * parameter. After a shadow is specified, all objects drawn subsequently
 * will be shadowed.  To turn off shadowing, set the shadow color to a
 * fully transparent color (or pass NULL as the color), or use the standard
 * gsave/grestore mechanism. }

procedure CGContextSetShadowWithColor( context: CGContextRef; offset: CGSize; blur: Float32; color: CGColorRef ); external name '_CGContextSetShadowWithColor'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Equivalent to calling
 *   CGContextSetShadowWithColor(context, offset, blur, color)
 * where color is black with 1/3 alpha (i.e., RGBA = (0, 0, 0, 1.0/3.0)) in
 * the DeviceRGB colorspace. }

procedure CGContextSetShadow( context: CGContextRef; offset: CGSize; blur: Float32 ); external name '_CGContextSetShadow'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{* Shading functions. *}

{ Fill the current clipping region of `c' with `shading'. }

procedure CGContextDrawShading( c: CGContextRef; shading: CGShadingRef ); external name '_CGContextDrawShading'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{* Text functions. *}

{ Set the current character spacing in the context `c' to `spacing'.  The
 * character spacing is added to the displacement between the origin of one
 * character and the origin of the next. }

procedure CGContextSetCharacterSpacing( c: CGContextRef; spacing: Float32 ); external name '_CGContextSetCharacterSpacing';

{ Set the user-space point at which text will be drawn in the context `c'
 * to `(x, y)'. }

procedure CGContextSetTextPosition( c: CGContextRef; x: Float32; y: Float32 ); external name '_CGContextSetTextPosition';

{ Return the user-space point at which text will be drawn in the context
 * `c'. }

function CGContextGetTextPosition( c: CGContextRef ): CGPoint; external name '_CGContextGetTextPosition';

{ Set the text matrix in the context `c' to `t'. }

procedure CGContextSetTextMatrix( c: CGContextRef; t: CGAffineTransform ); external name '_CGContextSetTextMatrix';

{ Return the text matrix in the context `c'. }

function CGContextGetTextMatrix( c: CGContextRef ): CGAffineTransform; external name '_CGContextGetTextMatrix';

{ Set the text drawing mode in the current graphics state of the context
 * `c' to `mode'. }

procedure CGContextSetTextDrawingMode( c: CGContextRef; mode: CGTextDrawingMode ); external name '_CGContextSetTextDrawingMode';

{ Set the font in the current graphics state of the context `c' to
 * `font'. }

procedure CGContextSetFont( c: CGContextRef; font: CGFontRef ); external name '_CGContextSetFont';

{ Set the font size in the current graphics state of the context `c' to
 * `size'. }

procedure CGContextSetFontSize( c: CGContextRef; size: Float32 ); external name '_CGContextSetFontSize';

{ Attempts to find the font named `name' and, if successful, sets it as
 * the font in the current graphics state of `c' and sets the font size in
 * the current graphics state to `size'. `textEncoding' specifies how to
 * translate from bytes to glyphs when displaying text. }

procedure CGContextSelectFont( c: CGContextRef; name: ConstCStringPtr; size: Float32; textEncoding: CGTextEncoding ); external name '_CGContextSelectFont';

{ Draw `string', a string of `length' bytes, at the point specified by the
 * text matrix in the context `c'.  Each byte of the string is mapped
 * through the encoding vector of the current font to obtain the glyph to
 * display. }

procedure CGContextShowText( c: CGContextRef; strng: ConstCStringPtr; length: size_t ); external name '_CGContextShowText';

{ Draw the glyphs pointed to by `g', an array of `count' glyphs, at the
 * point specified by the text matrix in the context `c'. }

procedure CGContextShowGlyphs( c: CGContextRef; {const} g: {variable-size-array} CGGlyphPtr; count: size_t ); external name '_CGContextShowGlyphs';

{ Draw `glyphs', an array of `count' CGGlyphs, at the current point
 * specified by the text matrix.  Each element of `advances' specifies the
 * offset from the previous glyph's origin to the origin of the associated
 * glyph; the advances are specified in user space. }

procedure CGContextShowGlyphsWithAdvances( c: CGContextRef; {const} glyphs: {variable-size-array} CGGlyphPtr; {const} advances: {variable-size-array} CGSizePtr; count: size_t ); external name '_CGContextShowGlyphsWithAdvances'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{* Text convenience functions. *}

{ Draw `string', a string of `length' bytes, at the point `(x, y)',
 * specified in user space, in the context `c'.  Each byte of the string is
 * mapped through the encoding vector of the current font to obtain the
 * glyph to display. }

procedure CGContextShowTextAtPoint( c: CGContextRef; x: Float32; y: Float32; strng: ConstCStringPtr; length: size_t ); external name '_CGContextShowTextAtPoint';

{ Display the glyphs pointed to by `glyphs', an array of `count' glyphs,
 * at at the point `(x, y)', specified in user space, in the context
 * `c'. }

procedure CGContextShowGlyphsAtPoint( c: CGContextRef; x: Float32; y: Float32; {const} glyphs: {variable-size-array} CGGlyphPtr; count: size_t ); external name '_CGContextShowGlyphsAtPoint';

{* PDF functions. *}

{ Draw `page' in the current user space of the context `c'. }

procedure CGContextDrawPDFPage( c: CGContextRef; page: CGPDFPageRef ); external name '_CGContextDrawPDFPage'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ DEPRECATED; use the CGPDFPage API instead.
 * Draw `page' in `document' in the rectangular area specified by `rect' in
 * the context `c'.  The media box of the page is scaled, if necessary, to
 * fit into `rect'. }

procedure CGContextDrawPDFDocument( c: CGContextRef; rect: CGRect; document: CGPDFDocumentRef; page: SInt32 ); external name '_CGContextDrawPDFDocument';

{* Output page functions. *}

{ Begin a new page. }

procedure CGContextBeginPage( c: CGContextRef; const (*var*) mediaBox: CGRect ); external name '_CGContextBeginPage';

{ End the current page. }

procedure CGContextEndPage( c: CGContextRef ); external name '_CGContextEndPage';

{* Context functions. *}

{ Equivalent to `CFRetain(c)'. }

function CGContextRetain( c: CGContextRef ): CGContextRef; external name '_CGContextRetain';

{ Equivalent to `CFRelease(c)'. }

procedure CGContextRelease( c: CGContextRef ); external name '_CGContextRelease';

{ Flush all drawing to the destination. }

procedure CGContextFlush( c: CGContextRef ); external name '_CGContextFlush';

{ Synchronized drawing. }

procedure CGContextSynchronize( c: CGContextRef ); external name '_CGContextSynchronize';

{* Antialiasing functions. *}

{ Turn on antialiasing if `shouldAntialias' is true; turn it off
 * otherwise.  This parameter is part of the graphics state. }

procedure CGContextSetShouldAntialias( c: CGContextRef; shouldAntialias: CBool ); external name '_CGContextSetShouldAntialias';

{ Allow antialiasing in context `c' if `allowsAntialiasing' is true; don't
 * allow it otherwise. This parameter is not part of the graphics state. A
 * context will perform antialiasing if both `allowsAntialiasing' and the
 * graphics state parameter `shouldAntialias' are true. }

procedure CGContextSetAllowsAntialiasing( context: CGContextRef; allowsAntialiasing: CBool ); external name '_CGContextSetAllowsAntialiasing'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{* Font smoothing functions. *}

{ Turn on font smoothing if `shouldSmoothFonts' is true; turn it off
 * otherwise.  This parameter is part of the graphics state. Note that this
 * doesn't guarantee that font smoothing will occur: not all destination
 * contexts support font smoothing. }

procedure CGContextSetShouldSmoothFonts( c: CGContextRef; shouldSmoothFonts: CBool ); external name '_CGContextSetShouldSmoothFonts'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{* Transparency layer support. *}

{ Begin a transparency layer.  All subsequent drawing operations until a
 * corresponding CGContextEndTransparencyLayer are composited into a fully
 * transparent backdrop (which is treated as a separate destination buffer
 * from the context); after a call to CGContextEndTransparencyLayer, the
 * result is composited into the context using the global alpha and shadow
 * state of the context.  This operation respects the clipping region of
 * the context.  After a call to this function, all of the parameters in
 * the graphics state remain unchanged with the exception of the following:
 *   The global alpha is set to 1.
 *   The shadow is turned off.
 * Ending the transparency layer restores these parameters to the values
 * they had before CGContextBeginTransparencyLayer was called.
 * Transparency layers may be nested. }

procedure CGContextBeginTransparencyLayer( context: CGContextRef; auxiliaryInfo: CFDictionaryRef ); external name '_CGContextBeginTransparencyLayer'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ End a tranparency layer. }

procedure CGContextEndTransparencyLayer( context: CGContextRef ); external name '_CGContextEndTransparencyLayer'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{* User space to device space tranformations. *}

{ Return the affine transform mapping the user space (abstract
 * coordinates) of `context' to device space (pixels). }

function CGContextGetUserSpaceToDeviceSpaceTransform( c: CGContextRef ): CGAffineTransform; external name '_CGContextGetUserSpaceToDeviceSpaceTransform'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Transform `point' from the user space of `context' to device space. }

function CGContextConvertPointToDeviceSpace( c: CGContextRef; point: CGPoint ): CGPoint; external name '_CGContextConvertPointToDeviceSpace'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Transform `point' from device space to the user space of `context'. }

function CGContextConvertPointToUserSpace( c: CGContextRef; point: CGPoint ): CGPoint; external name '_CGContextConvertPointToUserSpace'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Transform `size' from the user space of `context' to device space. }

function CGContextConvertSizeToDeviceSpace( c: CGContextRef; size: CGSize ): CGSize; external name '_CGContextConvertSizeToDeviceSpace'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Transform `size' from device space to the user space of `context'. }

function CGContextConvertSizeToUserSpace( c: CGContextRef; size: CGSize ): CGSize; external name '_CGContextConvertSizeToUserSpace'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Transform `rect' from the user space of `context' to device space. Since
 * affine transforms do not preserve rectangles in general, this function
 * returns the smallest rectangle which contains the transformed corner
 * points of `rect'. }

function CGContextConvertRectToDeviceSpace( c: CGContextRef; rect: CGRect ): CGRect; external name '_CGContextConvertRectToDeviceSpace'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Transform `rect' from device space to the user space of `context'. Since
 * affine transforms do not preserve rectangles in general, this function
 * returns the smallest rectangle which contains the transformed corner
 * points of `rect'. }

function CGContextConvertRectToUserSpace( c: CGContextRef; rect: CGRect ): CGRect; external name '_CGContextConvertRectToUserSpace'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.
