{ CoreGraphics - CGImage.h
 * Copyright (c) 2000-2004 Apple Computer, Inc.
 * All rights reserved.
 }
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
{       Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, November 2005 }
{       Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2007 }
{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit CGImage;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

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
uses MacTypes,CFBase,CGBase,CGGeometry,CGColorSpace,CGDataProvider;
{$ALIGN POWER}


type
	CGImageRef = ^SInt32; { an opaque 32-bit type }


type
	CGImageAlphaInfo = SInt32;
const
	kCGImageAlphaNone = 0;               { For example, RGB. }
	kCGImageAlphaPremultipliedLast = 1;  { For example, premultiplied RGBA }
	kCGImageAlphaPremultipliedFirst = 2; { For example, premultiplied ARGB }
	kCGImageAlphaLast = 3;               { For example, non-premultiplied RGBA }
	kCGImageAlphaFirst = 4;              { For example, non-premultiplied ARGB }
	kCGImageAlphaNoneSkipLast = 5;       { For example, RBGX. }
	kCGImageAlphaNoneSkipFirst = 6;      { For example, XRGB. }
	kCGImageAlphaOnly = 7;                { No color data, alpha data only }

const
	kCGBitmapAlphaInfoMask = $1F;
	kCGBitmapFloatComponents = 1 shl 8;
    
	kCGBitmapByteOrderMask = $7000;
	kCGBitmapByteOrder16Little = 1 shl 12;
	kCGBitmapByteOrder32Little = 2 shl 12;
	kCGBitmapByteOrder16Big = 3 shl 12;
	kCGBitmapByteOrder32Big = 4 shl 12;
type
	CGBitmapInfo = UInt32; { Available in MAC OS X 10.4 & later. }

const
{$ifc TARGET_RT_BIG_ENDIAN}
	kCGBitmapByteOrder16Host = kCGBitmapByteOrder16Big;
	kCGBitmapByteOrder32Host = kCGBitmapByteOrder32Big;
{$elsec}
	kCGBitmapByteOrder16Host = kCGBitmapByteOrder16Little;
	kCGBitmapByteOrder32Host = kCGBitmapByteOrder32Little;
{$endc}

{ Return the CFTypeID for CGImageRefs. }

function CGImageGetTypeID: CFTypeID; external name '_CGImageGetTypeID'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Create an image. }

function CGImageCreate( width: size_t; height: size_t; bitsPerComponent: size_t; bitsPerPixel: size_t; bytesPerRow: size_t; colorspace: CGColorSpaceRef; bitmapInfo: CGBitmapInfo; provider: CGDataProviderRef; {const} decode: {variable-size-array} Float32Ptr; shouldInterpolate: CBool; intent: CGColorRenderingIntent ): CGImageRef; external name '_CGImageCreate';

{ Create an image mask. }

function CGImageMaskCreate( width: size_t; height: size_t; bitsPerComponent: size_t; bitsPerPixel: size_t; bytesPerRow: size_t; provider: CGDataProviderRef; {const} decode: {variable-size-array} Float32Ptr; shouldInterpolate: CBool ): CGImageRef; external name '_CGImageMaskCreate';

{ Return a copy of `image'. Only the image structure itself is copied; the
 * underlying data is not. }

function CGImageCreateCopy( image: CGImageRef ): CGImageRef; external name '_CGImageCreateCopy';

{ Create an image from `source', a data provider of JPEG-encoded data. }

function CGImageCreateWithJPEGDataProvider( source: CGDataProviderRef; {const} decode: {variable-size-array} Float32Ptr; shouldInterpolate: CBool; intent: CGColorRenderingIntent ): CGImageRef; external name '_CGImageCreateWithJPEGDataProvider'; (* AVAILABLE_MAC_OS_X_VERSION_10_1_AND_LATER *)

{ Create an image using `source', a data provider for PNG-encoded data. }

function CGImageCreateWithPNGDataProvider( source: CGDataProviderRef; {const} decode: {variable-size-array} Float32Ptr; shouldInterpolate: CBool; intent: CGColorRenderingIntent ): CGImageRef; external name '_CGImageCreateWithPNGDataProvider'; (* AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER *)

{ Create an image using the data contained within the subrectangle `rect'
 * of `image'.
 *
 * The new image is created by
 *   1) adjusting `rect' to integral bounds by calling "CGRectIntegral";
 *   2) intersecting the result with a rectangle with origin (0, 0) and
 *      size equal to the size of `image';
 *   3) referencing the pixels within the resulting rectangle, treating the
 *      first pixel of the image data as the origin of the image.
 * If the resulting rectangle is the null rectangle, this function returns
 * NULL.
 *
 * If W and H are the width and height of image, respectively, then the
 * point (0,0) corresponds to the first pixel of the image data; the point
 * (W-1, 0) is the last pixel of the first row of the image data; (0, H-1)
 * is the first pixel of the last row of the image data; and (W-1, H-1) is
 * the last pixel of the last row of the image data.
 *
 * The resulting image retains a reference to the original image, so you
 * may release the original image after calling this function. }

function CGImageCreateWithImageInRect( image: CGImageRef; rect: CGRect ): CGImageRef; external name '_CGImageCreateWithImageInRect'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create a new image from `image' masked by `mask', which may be an image
 * mask or an image.
 *
 * If `mask' is an image mask, then it indicates which parts of the context
 * are to be painted with the image when drawn in a context, and which are
 * to be masked out (left unchanged). The source samples of the image mask
 * determine which areas are painted, acting as an "inverse alpha": if the
 * value of a source sample in the image mask is S, then the corresponding
 * region in `image' is blended with the destination using an alpha of
 * (1-S).  (For example, if S is 1, then the region is not painted, while
 * if S is 0, the region is fully painted.)
 *
 * If `mask' is an image, then it serves as alpha mask for blending the
 * image onto the destination.  The source samples of `mask' determine
 * which areas are painted: if the value of the source sample in mask is S,
 * then the corresponding region in image is blended with the destination
 * with an alpha of S.  (For example, if S is 0, then the region is not
 * painted, while if S is 1, the region is fully painted.)
 *
 * The parameter `image' may not be an image mask and may not have an image
 * mask or masking color associated with it.
 *
 * If `mask' is an image, then it must be in the DeviceGray color space,
 * may not have alpha, and may not itself be masked by an image mask
 * or a masking color. }

function CGImageCreateWithMask( image: CGImageRef; mask: CGImageRef ): CGImageRef; external name '_CGImageCreateWithMask'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create a new image from `image' masked by `components', an array of 2N
 * values ( min[1], max[1], ... min[N], max[N] ) where N is the number of
 * components in color space of `image'. Any image sample with color value
 * (c[1], ... c[N]) where min[i] <= c[i] <= max[i] for 1 <= i <= N is
 * masked out (that is, not painted).
 *
 * Each value in `components' must be a valid image sample value: if
 * `image' has integral pixel components, then each value of must be in the
 * range [0 .. 2**bitsPerComponent - 1] (where `bitsPerComponent' is the
 * number of bits/component of `image'); if `image' has floating-point
 * pixel components, then each value may be any floating-point number which
 * is a valid color component.
 *
 * The parameter `image' may not be an image mask, and may not already have
 * an image mask or masking color associated with it. }

function CGImageCreateWithMaskingColors( image: CGImageRef; {const} components: {variable-size-array} Float32Ptr ): CGImageRef; external name '_CGImageCreateWithMaskingColors'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create a copy of `image', replacing the image's colorspace with
 * `colorspace'.  Returns NULL if `image' is an image mask, or if the
 * number of components of `colorspace' isn't the same as the number of
 * components of the colorspace of `image'. }

function CGImageCreateCopyWithColorSpace( image: CGImageRef; colorspace: CGColorSpaceRef ): CGImageRef; external name '_CGImageCreateCopyWithColorSpace'; (* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

{ Equivalent to `CFRetain(image)'. }

function CGImageRetain( image: CGImageRef ): CGImageRef; external name '_CGImageRetain';

{ Equivalent to `CFRelease(image)'. }

procedure CGImageRelease( image: CGImageRef ); external name '_CGImageRelease';

{ Return true if `image' is an image mask, false otherwise. }

function CGImageIsMask( image: CGImageRef ): CBool; external name '_CGImageIsMask';

{ Return the width of `image'. }

function CGImageGetWidth( image: CGImageRef ): size_t; external name '_CGImageGetWidth';

{ Return the height of `image'. }

function CGImageGetHeight( image: CGImageRef ): size_t; external name '_CGImageGetHeight';

{ Return the number of bits/component of `image'. }

function CGImageGetBitsPerComponent( image: CGImageRef ): size_t; external name '_CGImageGetBitsPerComponent';

{ Return the number of bits/pixel of `image'. }

function CGImageGetBitsPerPixel( image: CGImageRef ): size_t; external name '_CGImageGetBitsPerPixel';

{ Return the number of bytes/row of `image'. }

function CGImageGetBytesPerRow( image: CGImageRef ): size_t; external name '_CGImageGetBytesPerRow';

{ Return the colorspace of `image', or NULL if `image' is an image
 * mask. }

function CGImageGetColorSpace( image: CGImageRef ): CGColorSpaceRef; external name '_CGImageGetColorSpace';

{ Return the alpha info of `image'. }

function CGImageGetAlphaInfo( image: CGImageRef ): CGImageAlphaInfo; external name '_CGImageGetAlphaInfo';

{ Return the data provider of `image'. }

function CGImageGetDataProvider( image: CGImageRef ): CGDataProviderRef; external name '_CGImageGetDataProvider';

{ Return the decode array of `image'. }

function CGImageGetDecode( image: CGImageRef ): Float32Ptr; external name '_CGImageGetDecode';

{ Return the interpolation parameter of `image'. }

function CGImageGetShouldInterpolate( image: CGImageRef ): CBool; external name '_CGImageGetShouldInterpolate';

{ Return the rendering intent of `image'. }

function CGImageGetRenderingIntent( image: CGImageRef ): CGColorRenderingIntent; external name '_CGImageGetRenderingIntent';

{ Return the bitmap info of `image'. }

function CGImageGetBitmapInfo( image: CGImageRef ): CGBitmapInfo; external name '_CGImageGetBitmapInfo'; (* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


end.
