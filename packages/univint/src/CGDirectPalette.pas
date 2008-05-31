{
 *  CGDirectPalette.h
 *  CoreGraphics
 *
 *  Copyright (c) 2000 Apple Computer, Inc. All rights reserved.
 *
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

unit CGDirectPalette;
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
uses MacTypes,CGDirectDisplay;
{$ALIGN POWER}


type
	CGPaletteBlendFraction = Float32; { A value between 0.0 and 1.0 }

{
 * Convenient device color representation
 *
 * Values should be in the range from 0.0 to 1.0, where 0.0 is black, and 1.0
 * is full on for each channel.
 }
type
	CGDeviceColorPtr = ^CGDeviceColor;
	CGDeviceColor = record
		red: Float32;
		green: Float32;
		blue: Float32;
	end;

	CGDeviceByteColorPtr = ^CGDeviceByteColor;
	CGDeviceByteColor = record
		red: SInt8;
		green: SInt8;
		blue: SInt8;
	end;

{
 * Create a new palette object representing the default 8 bit color palette.
 * Release the palette using CGPaletteRelease().
 }
function CGPaletteCreateDefaultColorPalette: CGDirectPaletteRef; external name '_CGPaletteCreateDefaultColorPalette';

{
 * Create a copy of the display's current palette, if any.
 * Returns NULL if the current display mode does not support a palette.
 * Release the palette using CGPaletteRelease().
 }
function CGPaletteCreateWithDisplay( display: CGDirectDisplayID ): CGDirectPaletteRef; external name '_CGPaletteCreateWithDisplay';

{
 * Create a new palette with a capacity as specified.  Entries are initialized from
 * the default color palette.  Release the palette using CGPaletteRelease().
 }
function CGPaletteCreateWithCapacity( capacity: CGTableCount ): CGDirectPaletteRef; external name '_CGPaletteCreateWithCapacity';

{
 * Create a new palette with a capacity and contents as specified.
 * Release the palette using CGPaletteRelease().
 }
function CGPaletteCreateWithSamples( var sampleTable: CGDeviceColor; sampleCount: CGTableCount ): CGDirectPaletteRef; external name '_CGPaletteCreateWithSamples';

{
 * Convenience function:
 * Create a new palette with a capacity and contents as specified.
 * Release the palette using CGPaletteRelease().
 }
function CGPaletteCreateWithByteSamples( var sampleTable: CGDeviceByteColor; sampleCount: CGTableCount ): CGDirectPaletteRef; external name '_CGPaletteCreateWithByteSamples';

{
 * Release a palette
 }
procedure CGPaletteRelease( palette: CGDirectPaletteRef ); external name '_CGPaletteRelease';

{
 * Get the color value at the specified index
 }
function CGPaletteGetColorAtIndex( palette: CGDirectPaletteRef; index: CGTableCount ): CGDeviceColor; external name '_CGPaletteGetColorAtIndex';

{
 * Get the index for the specified color value
 * The index returned is for a palette color with the
 * lowest RMS error to the specified color.
 }
function CGPaletteGetIndexForColor( palette: CGDirectPaletteRef; color: CGDeviceColor ): CGTableCount; external name '_CGPaletteGetIndexForColor';

{
 * Get the number of samples in the palette
 }
function CGPaletteGetNumberOfSamples( palette: CGDirectPaletteRef ): CGTableCount; external name '_CGPaletteGetNumberOfSamples';


{
 * Set the color value at the specified index
 }
procedure CGPaletteSetColorAtIndex( palette: CGDirectPaletteRef; color: CGDeviceColor; index: CGTableCount ); external name '_CGPaletteSetColorAtIndex';

{
 * Copy a palette
 }
function CGPaletteCreateCopy( palette: CGDirectPaletteRef ): CGDirectPaletteRef; external name '_CGPaletteCreateCopy';

{
 * Compare two palettes
 }
function CGPaletteIsEqualToPalette( palette1: CGDirectPaletteRef; palette2: CGDirectPaletteRef ): Boolean; external name '_CGPaletteIsEqualToPalette';

{
 * Create a new palette blended with a fraction of a device color.
 * Free the resulting palette with CGPaletteRelease()
 }
function CGPaletteCreateFromPaletteBlendedWithColor( palette: CGDirectPaletteRef; fraction: CGPaletteBlendFraction; color: CGDeviceColor ): CGDirectPaletteRef; external name '_CGPaletteCreateFromPaletteBlendedWithColor';


end.
