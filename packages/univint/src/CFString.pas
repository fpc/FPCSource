{	CFString.h
	Copyright (c) 1998-2005, Apple, Inc. All rights reserved.
}
{	  Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, November 2005 }
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

unit CFString;
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
uses MacTypes,CFBase,CFArray,CFData,CFDictionary,CFCharacterSet,CFLocale;
{$ALIGN POWER}


{
Please note: CFStrings are conceptually an array of Unicode characters.
However, in general, how a CFString stores this array is an implementation
detail. For instance, CFString might choose to use an array of 8-bit characters;
to store its contents; or it might use multiple blocks of memory; or whatever.
Furthermore, the implementation might change depending on the default
system encoding, the user's language, the OS, or even a given release.

What this means is that you should use the following advanced functions with care:

  CFStringGetPascalStringPtr()
  CFStringGetCStringPtr()
  CFStringGetCharactersPtr()

These functions are provided for optimization only. They will either return the desired
pointer quickly, in constant time, or they return NULL. They might choose to return NULL
for many reasons; for instance it's possible that for users running in different
languages these sometimes return NULL; or in a future OS release the first two might
switch to always returning NULL. Never observing NULL returns in your usages of these
functions does not mean they won't ever return NULL. (But note the CFStringGetCharactersPtr()
exception mentioned further below.)

In your usages of these functions, if you get a NULL return, use the non-Ptr version
of the functions as shown in this example:

  Str255 buffer;
  StringPtr ptr = CFStringGetPascalStringPtr(str, encoding);
  if (ptr == NULL) (
      if (CFStringGetPascalString(str, buffer, 256, encoding)) ptr = buffer;
  )

Note that CFStringGetPascalString() or CFStringGetCString() calls might still fail --- but
that will happen in two circumstances only: The conversion from the UniChar contents of CFString
to the specified encoding fails, or the buffer is too small. If they fail, that means
the conversion was not possible.

If you need a copy of the buffer in the above example, you might consider simply
calling CFStringGetPascalString() in all cases --- CFStringGetPascalStringPtr()
is simply an optimization.

In addition, the following functions, which create immutable CFStrings from developer
supplied buffers without copying the buffers, might have to actually copy
under certain circumstances (If they do copy, the buffer will be dealt with by the
"contentsDeallocator" argument.):

  CFStringCreateWithPascalStringNoCopy()
  CFStringCreateWithCStringNoCopy()
  CFStringCreateWithCharactersNoCopy()

You should of course never depend on the backing store of these CFStrings being
what you provided, and in other no circumstance should you change the contents
of that buffer (given that would break the invariant about the CFString being immutable).

Having said all this, there are actually ways to create a CFString where the backing store
is external, and can be manipulated by the developer or CFString itself:

  CFStringCreateMutableWithExternalCharactersNoCopy()
  CFStringSetExternalCharactersNoCopy()

A "contentsAllocator" is used to realloc or free the backing store by CFString.
kCFAllocatorNull can be provided to assure CFString will never realloc or free the buffer.
Developer can call CFStringSetExternalCharactersNoCopy() to update
CFString's idea of what's going on, if the buffer is changed externally. In these
strings, CFStringGetCharactersPtr() is guaranteed to return the external buffer.

These functions are here to allow wrapping a buffer of UniChar characters in a CFString,
allowing the buffer to passed into CFString functions and also manipulated via CFString
mutation functions. In general, developers should not use this technique for all strings,
as it prevents CFString from using certain optimizations.
}

{ Identifier for character encoding; the values are the same as Text Encoding Converter TextEncoding.
}
type
	CFStringEncoding = UInt32;
	CFStringEncodingPtr = ^CFStringEncoding;

{ Platform-independent built-in encodings; always available on all platforms.
   Call CFStringGetSystemEncoding() to get the default system encoding.
}
type
	CFStringBuiltInEncodings = SInt32;
const
	kCFStringEncodingInvalidId = $FFFFFFFF;
	kCFStringEncodingMacRoman = 0;
	kCFStringEncodingWindowsLatin1 = $0500; {  ANSI codepage 1252  }
	kCFStringEncodingISOLatin1 = $0201; {  ISO 8859-1  }
	kCFStringEncodingNextStepLatin = $0B01; {  NextStep encoding }
	kCFStringEncodingASCII = $0600; {  0..127 (in creating CFString, values greater than 0x7F are treated as corresponding Unicode value)  }
	kCFStringEncodingUnicode = $0100; {  kTextEncodingUnicodeDefault  + kTextEncodingDefaultFormat (aka kUnicode16BitFormat)  }
	kCFStringEncodingUTF8 = $08000100; {  kTextEncodingUnicodeDefault + kUnicodeUTF8Format  }
	kCFStringEncodingNonLossyASCII = $0BFF; {  7bit Unicode variants used by YellowBox & Java  }
{#if MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
	kCFStringEncodingUTF16 = $0100; { kTextEncodingUnicodeDefault + kUnicodeUTF16Format (alias of kCFStringEncodingUnicode) }
	kCFStringEncodingUTF16BE = $10000100; { kTextEncodingUnicodeDefault + kUnicodeUTF16BEFormat }
	kCFStringEncodingUTF16LE = $14000100; { kTextEncodingUnicodeDefault + kUnicodeUTF16LEFormat }

	kCFStringEncodingUTF32 = $0C000100; { kTextEncodingUnicodeDefault + kUnicodeUTF32Format }
	kCFStringEncodingUTF32BE = $18000100; { kTextEncodingUnicodeDefault + kUnicodeUTF32BEFormat }
	kCFStringEncodingUTF32LE = $1C000100; { kTextEncodingUnicodeDefault + kUnicodeUTF32LEFormat }
{#endif} { MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4 }

{ CFString type ID }
function CFStringGetTypeID: CFTypeID; external name '_CFStringGetTypeID';

{ Macro to allow creation of compile-time constant strings; the argument should be a constant string.

CFSTR(), not being a "Copy" or "Create" function, does not return a new
reference for you. So, you should not release the return value. This is
much like constant C or Pascal strings --- when you use "hello world"
in a program, you do not free it.

However, strings returned from CFSTR() can be retained and released in a
properly nested fashion, just like any other CF type. That is, if you pass
a CFSTR() return value to a function such as SetMenuItemWithCFString(), the
function can retain it, then later, when it's done with it, it can release it.

At this point non-7 bit characters (that is, characters > 127) in CFSTR() are not 
supported and using them will lead to unpredictable results. This includes escaped
(\nnn) characters whose values are > 127. Even if it works for you in testing, 
it might not work for a user with a different language preference.
}
{
	*** Pascal Usage of CFSTR et al ***
	
	For Pascal:
	
		To define your own constant CFStrings, call call CFSTRP with a 
		constant Pascal single-quoted string only, for example 
		CFSTRP('a constant string').

	For MetroWerks Pascal:
	
		Include the CFStringGlue file in your project (it defines the private function,
		CFSTRP0, which you should never call directly.
		
		In your uses clause, use CFString and CFStringGlue and whichever PInterface files
		defining the constant CFStrings (or define your own using CFSTRP('pascal string')).
	
	For GNU Pascal:
	
		Include the GPCMacros.inc file (for example $I GPCMacros.inc).
		
		In your uses clause, use CFString (and, optionally, CFStringGlue and the 
		PInterface files defining the constant CFStrings).
		
		You can optionally use CFSTR with a constant double-quoted C String (for example,
		CFSTR("a constant C string")) although it will make no difference, but will
		result in closer  C source code similarity.
	
	For maximum compatibility in both GPC and MetroWerks Pascal
	
		Include the GPCMacros.inc file in GPC as part of your GPC prefix.
		
		Include the CFStringGlue file in your MW project.
		
		Only ever use CFSTRP with a constant Pascal string.
		
		In your uses clause for a unit, use the CFString, CFStringGlue and any 
		PInterface files defining any constant CFStrings you need.
}

{
	CFSTR Must only be called with a constant CString
}
function CFSTR( c: PChar ): CFStringRef; external name '___CFStringMakeConstantString';
{
	CFSTRP Must only be called with a constant Pascal String
}
function CFSTRP( c: PChar ): CFStringRef; external name '___CFStringMakeConstantString';

{
	CFSTRP0 Must only be called with a constant Pascal String terminated with a chr(0)
}
function CFSTRP0( c: PChar ): CFStringRef; external name '___CFStringMakeConstantString';

{** Immutable string creation functions **}

{ Functions to create basic immutable strings. The provided allocator is used for all memory activity in these functions.
}

{ These functions copy the provided buffer into CFString's internal storage. }
function CFStringCreateWithPascalString( alloc: CFAllocatorRef; const (*var*) pStr: Str255; encoding: CFStringEncoding ): CFStringRef; external name '_CFStringCreateWithPascalString';

function CFStringCreateWithCString( alloc: CFAllocatorRef; cStr: ConstCStringPtr; encoding: CFStringEncoding ): CFStringRef; external name '_CFStringCreateWithCString';

function CFStringCreateWithCharacters( alloc: CFAllocatorRef; chars: UniCharPtr; numChars: CFIndex ): CFStringRef; external name '_CFStringCreateWithCharacters';

{ These functions try not to copy the provided buffer. The buffer will be deallocated 
with the provided contentsDeallocator when it's no longer needed; to not free
the buffer, specify kCFAllocatorNull here. As usual, NULL means default allocator.

NOTE: Do not count on these buffers as being used by the string; 
in some cases the CFString might free the buffer and use something else
(for instance if it decides to always use Unicode encoding internally). 

NOTE: If you are not transferring ownership of the buffer to the CFString
(for instance, you supplied contentsDeallocator = kCFAllocatorNull), it is your
responsibility to assure the buffer does not go away during the lifetime of the string.
If the string is retained or copied, its lifetime might extend in ways you cannot
predict. So, for strings created with buffers whose lifetimes you cannot
guarantee, you need to be extremely careful --- do not hand it out to any
APIs which might retain or copy the strings.
}
function CFStringCreateWithPascalStringNoCopy( alloc: CFAllocatorRef; const (*var*) pStr: Str255; encoding: CFStringEncoding; contentsDeallocator: CFAllocatorRef ): CFStringRef; external name '_CFStringCreateWithPascalStringNoCopy';

function CFStringCreateWithCStringNoCopy( alloc: CFAllocatorRef; cStr: ConstCStringPtr; encoding: CFStringEncoding; contentsDeallocator: CFAllocatorRef ): CFStringRef; external name '_CFStringCreateWithCStringNoCopy';

function CFStringCreateWithCharactersNoCopy( alloc: CFAllocatorRef; chars: UniCharPtr; numChars: CFIndex; contentsDeallocator: CFAllocatorRef ): CFStringRef; external name '_CFStringCreateWithCharactersNoCopy';

{ Create copies of part or all of the string.
}
function CFStringCreateWithSubstring( alloc: CFAllocatorRef; str: CFStringRef; range: CFRange ): CFStringRef; external name '_CFStringCreateWithSubstring';

function CFStringCreateCopy( alloc: CFAllocatorRef; theString: CFStringRef ): CFStringRef; external name '_CFStringCreateCopy';

{ These functions create a CFString from the provided printf-like format string and arguments.
}
function CFStringCreateWithFormat( alloc: CFAllocatorRef; formatOptions: CFDictionaryRef; format: CFStringRef; ... ): CFStringRef; external name '_CFStringCreateWithFormat';

function CFStringCreateWithFormatAndArguments( alloc: CFAllocatorRef; formatOptions: CFDictionaryRef; format: CFStringRef; arguments: UnivPtr ): CFStringRef; external name '_CFStringCreateWithFormatAndArguments';

{ Functions to create mutable strings. "maxLength", if not 0, is a hard bound on the length of the string. If 0, there is no limit on the length.
}
function CFStringCreateMutable( alloc: CFAllocatorRef; maxLength: CFIndex ): CFMutableStringRef; external name '_CFStringCreateMutable';

function CFStringCreateMutableCopy( alloc: CFAllocatorRef; maxLength: CFIndex; theString: CFStringRef ): CFMutableStringRef; external name '_CFStringCreateMutableCopy';

{ This function creates a mutable string that has a developer supplied and directly editable backing store.
The string will be manipulated within the provided buffer (if any) until it outgrows capacity; then the
externalCharactersAllocator will be consulted for more memory. When the CFString is deallocated, the
buffer will be freed with the externalCharactersAllocator. Provide kCFAllocatorNull here to prevent the buffer
from ever being reallocated or deallocated by CFString. See comments at top of this file for more info.
}
function CFStringCreateMutableWithExternalCharactersNoCopy( alloc: CFAllocatorRef; chars: UniCharPtr; numChars: CFIndex; capacity: CFIndex; externalCharactersAllocator: CFAllocatorRef ): CFMutableStringRef; external name '_CFStringCreateMutableWithExternalCharactersNoCopy';

{** Basic accessors for the contents **}

{ Number of 16-bit Unicode characters in the string.
}
function CFStringGetLength( theString: CFStringRef ): CFIndex; external name '_CFStringGetLength';

{ Extracting the contents of the string. For obtaining multiple characters, calling
CFStringGetCharacters() is more efficient than multiple calls to CFStringGetCharacterAtIndex().
If the length of the string is not known (so you can't use a fixed size buffer for CFStringGetCharacters()),
another method is to use is CFStringGetCharacterFromInlineBuffer() (see further below).
}
function CFStringGetCharacterAtIndex( theString: CFStringRef; idx: CFIndex ): UniChar; external name '_CFStringGetCharacterAtIndex';

procedure CFStringGetCharacters( theString: CFStringRef; range: CFRange; buffer: UniCharPtr ); external name '_CFStringGetCharacters';


{** Conversion to other encodings **}

{ These two convert into the provided buffer; they return false if conversion isn't possible
(due to conversion error, or not enough space in the provided buffer). 
These functions do zero-terminate or put the length byte; the provided bufferSize should include
space for this (so pass 256 for Str255). More sophisticated usages can go through CFStringGetBytes().
These functions are equivalent to calling CFStringGetBytes() with 
the range of the string; lossByte = 0; and isExternalRepresentation = false; 
if successful, they then insert the leading length of terminating zero, as desired.
}
function CFStringGetPascalString( theString: CFStringRef; buffer: StringPtr; bufferSize: CFIndex; encoding: CFStringEncoding ): Boolean; external name '_CFStringGetPascalString';

function CFStringGetCString( theString: CFStringRef; buffer: CStringPtr; bufferSize: CFIndex; encoding: CFStringEncoding ): Boolean; external name '_CFStringGetCString';

{ These functions attempt to return in O(1) time the desired format for the string.
Note that although this means a pointer to the internal structure is being returned,
this can't always be counted on. Please see note at the top of the file for more
details.
}
function CFStringGetPascalStringPtr( theString: CFStringRef; encoding: CFStringEncoding ): ConstStringPtr; external name '_CFStringGetPascalStringPtr'; { May return NULL at any time; be prepared for NULL }

function CFStringGetCStringPtr( theString: CFStringRef; encoding: CFStringEncoding ): ConstCStringPtr; external name '_CFStringGetCStringPtr'; { May return NULL at any time; be prepared for NULL }

function CFStringGetCharactersPtr( theString: CFStringRef ): UniCharPtr; external name '_CFStringGetCharactersPtr'; { May return NULL at any time; be prepared for NULL }

{ The primitive conversion routine; allows you to convert a string piece at a time
   into a fixed size buffer. Returns number of characters converted. 
   Characters that cannot be converted to the specified encoding are represented
   with the byte specified by lossByte; if lossByte is 0, then lossy conversion
   is not allowed and conversion stops, returning partial results.
   Pass buffer==NULL if you don't care about the converted string (but just the convertability,
   or number of bytes required). 
   maxBufLength indicates the maximum number of bytes to generate. It is ignored when buffer==NULL.
   Does not zero-terminate. If you want to create Pascal or C string, allow one extra byte at start or end. 
   Setting isExternalRepresentation causes any extra bytes that would allow 
   the data to be made persistent to be included; for instance, the Unicode BOM.
}
function CFStringGetBytes( theString: CFStringRef; range: CFRange; encoding: CFStringEncoding; lossByte: ByteParameter; isExternalRepresentation: Boolean; buffer: UInt8Ptr; maxBufLen: CFIndex; var usedBufLen: CFIndex ): CFIndex; external name '_CFStringGetBytes';

{ This one goes the other way by creating a CFString from a bag of bytes.
This is much like CFStringCreateWithPascalString or CFStringCreateWithCString,
except the length is supplied explicitly. In addition, you can specify whether
the data is an external format --- that is, whether to pay attention to the
BOM character (if any) and do byte swapping if necessary
}
function CFStringCreateWithBytes( alloc: CFAllocatorRef; bytes: UnivPtr; numBytes: CFIndex; encoding: CFStringEncoding; isExternalRepresentation: Boolean ): CFStringRef; external name '_CFStringCreateWithBytes';

{ Convenience functions String <-> Data. These generate "external" formats, that is, formats that
   can be written out to disk. For instance, if the encoding is Unicode, CFStringCreateFromExternalRepresentation()
   pays attention to the BOM character (if any) and does byte swapping if necessary.
   Similarly CFStringCreateExternalRepresentation() will always include a BOM character if the encoding is
   Unicode. See above for description of lossByte.
}
function CFStringCreateFromExternalRepresentation( alloc: CFAllocatorRef; data: CFDataRef; encoding: CFStringEncoding ): CFStringRef; external name '_CFStringCreateFromExternalRepresentation'; { May return NULL on conversion error }

function CFStringCreateExternalRepresentation( alloc: CFAllocatorRef; theString: CFStringRef; encoding: CFStringEncoding; lossByte: ByteParameter ): CFDataRef; external name '_CFStringCreateExternalRepresentation'; { May return NULL on conversion error }

{ Hints about the contents of a string
}
function CFStringGetSmallestEncoding( theString: CFStringRef ): CFStringEncoding; external name '_CFStringGetSmallestEncoding'; { Result in O(n) time max }

function CFStringGetFastestEncoding( theString: CFStringRef ): CFStringEncoding; external name '_CFStringGetFastestEncoding'; { Result in O(1) time max }

{ General encoding info
}
function CFStringGetSystemEncoding: CFStringEncoding; external name '_CFStringGetSystemEncoding'; { The default encoding for the system; untagged 8-bit characters are usually in this encoding }

function CFStringGetMaximumSizeForEncoding( length: CFIndex; encoding: CFStringEncoding ): CFIndex; external name '_CFStringGetMaximumSizeForEncoding'; { Max bytes a string of specified length (in UniChars) will take up if encoded }


{** FileSystem path conversion functions **}

{ Extract the contents of the string as a NULL-terminated 8-bit string appropriate for passing to POSIX APIs.  The string is zero-terminated. false will be returned if the conversion results don't fit into the buffer.  Use CFStringGetMaximumSizeOfFileSystemRepresentation() if you want to make sure the buffer is of sufficient length.
}
function CFStringGetFileSystemRepresentation( strng: CFStringRef; buffer: CStringPtr; maxBufLen: CFIndex ): Boolean; external name '_CFStringGetFileSystemRepresentation';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Get the upper bound on the number of bytes required to hold the file system representation for the string. This result is returned quickly as a very rough approximation, and could be much larger than the actual space required. The result includes space for the zero termination. If you are allocating a buffer for long-term keeping, it's recommended that you reallocate it smaller (to be the right size) after calling CFStringGetFileSystemRepresentation(). 
}
function CFStringGetMaximumSizeOfFileSystemRepresentation( strng: CFStringRef ): CFIndex; external name '_CFStringGetMaximumSizeOfFileSystemRepresentation';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Create a CFString from the specified zero-terminated POSIX file system representation.  If the conversion fails (possible due to bytes in the buffer not being a valid sequence of bytes for the appropriate character encoding), NULL is returned.
}
function CFStringCreateWithFileSystemRepresentation( alloc: CFAllocatorRef; buffer: ConstCStringPtr ): CFStringRef; external name '_CFStringCreateWithFileSystemRepresentation';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{** Comparison functions. **}

{ Find and compare flags; these are OR'ed together as compareOptions or searchOptions in the various functions. 
   This typedef doesn't appear in the functions; instead the argument is CFOptionFlags. 
}
type
	CFStringCompareFlags = SInt32;
const
																{  Flags used in all find and compare operations  }
	kCFCompareCaseInsensitive = 1;
	kCFCompareBackwards = 4;							{  Starting from the end of the string  }
	kCFCompareAnchored = 8;							{  Only at the specified starting point  }
	kCFCompareNonliteral = 16;							{  If specified, loose equivalence is performed (o-umlaut == o, umlaut)  }
	kCFCompareLocalized = 32;							{  User's default locale is used for the comparisons  }
	kCFCompareNumerically = 64;							{  Numeric comparison is used; that is, Foo2.txt < Foo7.txt < Foo25.txt  }

{ The main comparison routine; compares specified range of the first string to (the full range of) the second string.
   locale == NULL indicates canonical locale.
   kCFCompareNumerically, added in 10.2, does not work if kCFCompareLocalized is specified on systems before 10.3
   kCFCompareBackwards and kCFCompareAnchored are not applicable.
}
function CFStringCompareWithOptions( theString1: CFStringRef; theString2: CFStringRef; rangeToCompare: CFRange; compareOptions: CFOptionFlags ): CFComparisonResult; external name '_CFStringCompareWithOptions';

{ Comparison convenience suitable for passing as sorting functions.
   kCFCompareNumerically, added in 10.2, does not work if kCFCompareLocalized is specified on systems before 10.3
   kCFCompareBackwards and kCFCompareAnchored are not applicable.
}
function CFStringCompare( theString1: CFStringRef; theString2: CFStringRef; compareOptions: CFOptionFlags ): CFComparisonResult; external name '_CFStringCompare';

{ CFStringFindWithOptions() returns the found range in the CFRange * argument; you can pass NULL for simple discovery check.
   If stringToFind is the empty string (zero length), nothing is found.
   Ignores the kCFCompareNumerically option.
}
function CFStringFindWithOptions( theString: CFStringRef; stringToFind: CFStringRef; rangeToSearch: CFRange; searchOptions: CFOptionFlags; var result: CFRange ): Boolean; external name '_CFStringFindWithOptions';

{ CFStringCreateArrayWithFindResults() returns an array of CFRange pointers, or NULL if there are no matches.
   Overlapping instances are not found; so looking for "AA" in "AAA" finds just one range.
   Post 10.1: If kCFCompareBackwards is provided, the scan is done from the end (which can give a different result), and
      the results are stored in the array backwards (last found range in slot 0).
   If stringToFind is the empty string (zero length), nothing is found.
   kCFCompareAnchored causes just the consecutive instances at start (or end, if kCFCompareBackwards) to be reported. So, searching for "AB" in "ABABXAB..." you just get the first two occurrences.
   Ignores the kCFCompareNumerically option.
}
function CFStringCreateArrayWithFindResults( alloc: CFAllocatorRef; theString: CFStringRef; stringToFind: CFStringRef; rangeToSearch: CFRange; compareOptions: CFOptionFlags ): CFArrayRef; external name '_CFStringCreateArrayWithFindResults';

{ Find conveniences; see comments above concerning empty string and options.
}
function CFStringFind( theString: CFStringRef; stringToFind: CFStringRef; compareOptions: CFOptionFlags ): CFRange; external name '_CFStringFind';

function CFStringHasPrefix( theString: CFStringRef; prefix: CFStringRef ): Boolean; external name '_CFStringHasPrefix';

function CFStringHasSuffix( theString: CFStringRef; suffix: CFStringRef ): Boolean; external name '_CFStringHasSuffix';

{#if MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
{!
	@function CFStringGetRangeOfComposedCharactersAtIndex
	Returns the range of the composed character sequence at the specified index.
	@param theString The CFString which is to be searched.  If this
                		parameter is not a valid CFString, the behavior is
              		undefined.
	@param theIndex The index of the character contained in the
			composed character sequence.  If the index is
			outside the index space of the string (0 to N-1 inclusive,
			where N is the length of the string), the behavior is
			undefined.
	@result The range of the composed character sequence.
}
function CFStringGetRangeOfComposedCharactersAtIndex( theString: CFStringRef; theIndex: CFIndex ): CFRange; external name '_CFStringGetRangeOfComposedCharactersAtIndex';

{!
	@function CFStringFindCharacterFromSet
	Query the range of the first character contained in the specified character set.
	@param theString The CFString which is to be searched.  If this
                		parameter is not a valid CFString, the behavior is
              		undefined.
	@param theSet The CFCharacterSet against which the membership
			of characters is checked.  If this parameter is not a valid
			CFCharacterSet, the behavior is undefined.
	@param range The range of characters within the string to search. If
			the range location or end point (defined by the location
			plus length minus 1) are outside the index space of the
			string (0 to N-1 inclusive, where N is the length of the
			string), the behavior is undefined. If the range length is
			negative, the behavior is undefined. The range may be empty
			(length 0), in which case no search is performed.
	@param searchOptions The bitwise-or'ed option flags to control
			the search behavior.  The supported options are
			kCFCompareBackwards andkCFCompareAnchored.
			If other option flags are specified, the behavior
                        is undefined.
	@param result The pointer to a CFRange supplied by the caller in
			which the search result is stored.  Note that the length
                        of this range could be more than If a pointer to an invalid
			memory is specified, the behavior is undefined.
	@result true, if at least a character which is a member of the character
			set is found and result is filled, otherwise, false.
}
function CFStringFindCharacterFromSet( theString: CFStringRef; theSet: CFCharacterSetRef; rangeToSearch: CFRange; searchOptions: CFOptionFlags; var result: CFRange ): Boolean; external name '_CFStringFindCharacterFromSet';
{#endif}

{ Find range of bounds of the line(s) that span the indicated range (startIndex, numChars),
   taking into account various possible line separator sequences (CR, CRLF, LF, and Unicode LS, PS).
   All return values are "optional" (provide NULL if you don't want them)
     lineStartIndex: index of first character in line
     lineEndIndex: index of first character of the next line (including terminating line separator characters)
     contentsEndIndex: index of the first line separator character
   Thus, lineEndIndex - lineStartIndex is the number of chars in the line, including the line separators
         contentsEndIndex - lineStartIndex is the number of chars in the line w/out the line separators
}
procedure CFStringGetLineBounds( theString: CFStringRef; range: CFRange; var lineBeginIndex: CFIndex; var lineEndIndex: CFIndex; var contentsEndIndex: CFIndex ); external name '_CFStringGetLineBounds';


{** Exploding and joining strings with a separator string **}

function CFStringCreateByCombiningStrings( alloc: CFAllocatorRef; theArray: CFArrayRef; separatorString: CFStringRef ): CFStringRef; external name '_CFStringCreateByCombiningStrings'; { Empty array returns empty string; one element array returns the element }

function CFStringCreateArrayBySeparatingStrings( alloc: CFAllocatorRef; theString: CFStringRef; separatorString: CFStringRef ): CFArrayRef; external name '_CFStringCreateArrayBySeparatingStrings'; { No separators in the string returns array with that string; string == sep returns two empty strings }


{** Parsing non-localized numbers from strings **}

function CFStringGetIntValue( str: CFStringRef ): SInt32; external name '_CFStringGetIntValue'; { Skips whitespace; returns 0 on error, MAX or -MAX on overflow }

function CFStringGetDoubleValue( str: CFStringRef ): Float64; external name '_CFStringGetDoubleValue'; { Skips whitespace; returns 0.0 on error }


{** MutableString functions **}

{ CFStringAppend("abcdef", "xxxxx") -> "abcdefxxxxx"
   CFStringDelete("abcdef", CFRangeMake(2, 3)) -> "abf"
   CFStringReplace("abcdef", CFRangeMake(2, 3), "xxxxx") -> "abxxxxxf"
   CFStringReplaceAll("abcdef", "xxxxx") -> "xxxxx"
}
procedure CFStringAppend( theString: CFMutableStringRef; appendedString: CFStringRef ); external name '_CFStringAppend';

procedure CFStringAppendCharacters( theString: CFMutableStringRef; chars: UniCharPtr; numChars: CFIndex ); external name '_CFStringAppendCharacters';

procedure CFStringAppendPascalString( theString: CFMutableStringRef; const (*var*) pStr: Str255; encoding: CFStringEncoding ); external name '_CFStringAppendPascalString';

procedure CFStringAppendCString( theString: CFMutableStringRef; cStr: ConstCStringPtr; encoding: CFStringEncoding ); external name '_CFStringAppendCString';

procedure CFStringAppendFormat( theString: CFMutableStringRef; formatOptions: CFDictionaryRef; format: CFStringRef; ... ); external name '_CFStringAppendFormat';

procedure CFStringAppendFormatAndArguments( theString: CFMutableStringRef; formatOptions: CFDictionaryRef; format: CFStringRef; arguments: UnivPtr ); external name '_CFStringAppendFormatAndArguments';

procedure CFStringInsert( str: CFMutableStringRef; idx: CFIndex; insertedStr: CFStringRef ); external name '_CFStringInsert';

procedure CFStringDelete( theString: CFMutableStringRef; range: CFRange ); external name '_CFStringDelete';

procedure CFStringReplace( theString: CFMutableStringRef; range: CFRange; replacement: CFStringRef ); external name '_CFStringReplace';

procedure CFStringReplaceAll( theString: CFMutableStringRef; replacement: CFStringRef ); external name '_CFStringReplaceAll'; { Replaces whole string }

{#if MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
{ Replace all occurrences of target in rangeToSearch of theString with replacement.
   Pays attention to kCFCompareCaseInsensitive, kCFCompareBackwards, kCFCompareNonliteral, and kCFCompareAnchored.
   kCFCompareBackwards can be used to do the replacement starting from the end, which could give a different result.
     ex. AAAAA, replace AA with B -> BBA or ABB; latter if kCFCompareBackwards
   kCFCompareAnchored assures only anchored but multiple instances are found (the instances must be consecutive at start or end)
     ex. AAXAA, replace A with B -> BBXBB or BBXAA; latter if kCFCompareAnchored
   Returns number of replacements performed.
}
function CFStringFindAndReplace( theString: CFMutableStringRef; stringToFind: CFStringRef; replacementString: CFStringRef; rangeToSearch: CFRange; compareOptions: CFOptionFlags ): CFIndex; external name '_CFStringFindAndReplace';

{#endif}

{ This function will make the contents of a mutable CFString point directly at the specified UniChar array.
   It works only with CFStrings created with CFStringCreateMutableWithExternalCharactersNoCopy().
   This function does not free the previous buffer.
   The string will be manipulated within the provided buffer (if any) until it outgrows capacity; then the
     externalCharactersAllocator will be consulted for more memory.
   See comments at the top of this file for more info.
}
procedure CFStringSetExternalCharactersNoCopy( theString: CFMutableStringRef; chars: UniCharPtr; length: CFIndex; capacity: CFIndex ); external name '_CFStringSetExternalCharactersNoCopy'; { Works only on specially created mutable strings! }

{ CFStringPad() will pad or cut down a string to the specified size.
   The pad string is used as the fill string; indexIntoPad specifies which character to start with.
     CFStringPad("abc", " ", 9, 0) ->  "abc      "
     CFStringPad("abc", ". ", 9, 1) -> "abc . . ."
     CFStringPad("abcdef", ?, 3, ?) -> "abc"

     CFStringTrim() will trim the specified string from both ends of the string.
     CFStringTrimWhitespace() will do the same with white space characters (tab, newline, etc)
     CFStringTrim("  abc ", " ") -> "abc"
     CFStringTrim("* * * *abc * ", "* ") -> "*abc "
}
procedure CFStringPad( theString: CFMutableStringRef; padString: CFStringRef; length: CFIndex; indexIntoPad: CFIndex ); external name '_CFStringPad';

procedure CFStringTrim( theString: CFMutableStringRef; trimString: CFStringRef ); external name '_CFStringTrim';

procedure CFStringTrimWhitespace( theString: CFMutableStringRef ); external name '_CFStringTrimWhitespace';

{#if MAC_OS_X_VERSION_10_3 <= MAC_OS_X_VERSION_MAX_ALLOWED}
procedure CFStringLowercase( theString: CFMutableStringRef; locale: CFLocaleRef ); external name '_CFStringLowercase';

procedure CFStringUppercase( theString: CFMutableStringRef; locale: CFLocaleRef ); external name '_CFStringUppercase';

procedure CFStringCapitalize( theString: CFMutableStringRef; locale: CFLocaleRef ); external name '_CFStringCapitalize';
{#else}
//procedure CFStringLowercase( theString: CFMutableStringRef; localeTBD: {const} UnivPtr ); // localeTBD must be NULL on pre-10.3

//procedure CFStringUppercase( theString: CFMutableStringRef; localeTBD: {const} UnivPtr ); // localeTBD must be NULL on pre-10.3

//procedure CFStringCapitalize( theString: CFMutableStringRef; localeTBD: {const} UnivPtr ); // localeTBD must be NULL on pre-10.3
{#endif}

{#if MAC_OS_X_VERSION_10_2 <= MAC_OS_X_VERSION_MAX_ALLOWED}
{!
	@typedef CFStringNormalizationForm
	This is the type of Unicode normalization forms as described in
	Unicode Technical Report #15.
}
type
	CFStringNormalizationForm = SInt32;
const
	kCFStringNormalizationFormD = 0; // Canonical Decomposition
	kCFStringNormalizationFormKD = 1; // Compatibility Decomposition
	kCFStringNormalizationFormC = 2; // Canonical Decomposition followed by Canonical Composition
	kCFStringNormalizationFormKC = 3; // Compatibility Decomposition followed by Canonical Composition

{!
	@function CFStringNormalize
	Normalizes the string into the specified form as described in
	Unicode Technical Report #15.
	@param theString  The string which is to be normalized.  If this
		parameter is not a valid mutable CFString, the behavior is
		undefined.
	@param theForm  The form into which the string is to be normalized.
		If this parameter is not a valid CFStringNormalizationForm value,
		the behavior is undefined.
}
procedure CFStringNormalize( theString: CFMutableStringRef; theForm: CFStringNormalizationForm ); external name '_CFStringNormalize';
{#endif}

{ Perform string transliteration.  The transformation represented by transform (see below for the full list of transforms supported) is applied to the given range of string, modifying it in place. Only the specified range will be modified, but the transform may look at portions of the string outside that range for context. NULL range pointer causes the whole string to be transformed. On return, range is modified to reflect the new range corresponding to the original range. reverse indicates that the inverse transform should be used instead, if it exists. If the transform is successful, true is returned; if unsuccessful, false. Reasons for the transform being unsuccessful include an invalid transform identifier, or attempting to reverse an irreversible transform.
}
function CFStringTransform( strng: CFMutableStringRef; var range: CFRange; transform: CFStringRef; reverse: Boolean ): Boolean; external name '_CFStringTransform';
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)

{ Transform identifiers for CFStringTransform()
}
var kCFStringTransformStripCombiningMarks: CFStringRef; external name '_kCFStringTransformStripCombiningMarks'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformToLatin: CFStringRef; external name '_kCFStringTransformToLatin'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformFullwidthHalfwidth: CFStringRef; external name '_kCFStringTransformFullwidthHalfwidth'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformLatinKatakana: CFStringRef; external name '_kCFStringTransformLatinKatakana'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformLatinHiragana: CFStringRef; external name '_kCFStringTransformLatinHiragana'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformHiraganaKatakana: CFStringRef; external name '_kCFStringTransformHiraganaKatakana'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformMandarinLatin: CFStringRef; external name '_kCFStringTransformMandarinLatin'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformLatinHangul: CFStringRef; external name '_kCFStringTransformLatinHangul'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformLatinArabic: CFStringRef; external name '_kCFStringTransformLatinArabic'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformLatinHebrew: CFStringRef; external name '_kCFStringTransformLatinHebrew'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformLatinThai: CFStringRef; external name '_kCFStringTransformLatinThai'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformLatinCyrillic: CFStringRef; external name '_kCFStringTransformLatinCyrillic'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformLatinGreek: CFStringRef; external name '_kCFStringTransformLatinGreek'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformToXMLHex: CFStringRef; external name '_kCFStringTransformToXMLHex'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)
var kCFStringTransformToUnicodeName: CFStringRef; external name '_kCFStringTransformToUnicodeName'; (* attribute const *)
(* AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER *)


{** General encoding related functionality **}

{ This returns availability of the encoding on the system
}
function CFStringIsEncodingAvailable( encoding: CFStringEncoding ): Boolean; external name '_CFStringIsEncodingAvailable';

{ This function returns list of available encodings.  The returned list is terminated with kCFStringEncodingInvalidId and owned by the system.
}
function CFStringGetListOfAvailableEncodings: CFStringEncodingPtr; external name '_CFStringGetListOfAvailableEncodings';

{ Returns name of the encoding; non-localized.
}
function CFStringGetNameOfEncoding( encoding: CFStringEncoding ): CFStringRef; external name '_CFStringGetNameOfEncoding';

{ ID mapping functions from/to Cocoa NSStringEncoding.  Returns kCFStringEncodingInvalidId if no mapping exists.
}
function CFStringConvertEncodingToNSStringEncoding( encoding: CFStringEncoding ): UInt32; external name '_CFStringConvertEncodingToNSStringEncoding';

function CFStringConvertNSStringEncodingToEncoding( encoding: UInt32 ): CFStringEncoding; external name '_CFStringConvertNSStringEncodingToEncoding';

{ ID mapping functions from/to Microsoft Windows codepage (covers both OEM & ANSI).  Returns kCFStringEncodingInvalidId if no mapping exists.
}
function CFStringConvertEncodingToWindowsCodepage( encoding: CFStringEncoding ): UInt32; external name '_CFStringConvertEncodingToWindowsCodepage';

function CFStringConvertWindowsCodepageToEncoding( codepage: UInt32 ): CFStringEncoding; external name '_CFStringConvertWindowsCodepageToEncoding';

{ ID mapping functions from/to IANA registery charset names.  Returns kCFStringEncodingInvalidId if no mapping exists.
}
function CFStringConvertIANACharSetNameToEncoding( theString: CFStringRef ): CFStringEncoding; external name '_CFStringConvertIANACharSetNameToEncoding';

function CFStringConvertEncodingToIANACharSetName( encoding: CFStringEncoding ): CFStringRef; external name '_CFStringConvertEncodingToIANACharSetName';

{ Returns the most compatible MacOS script value for the input encoding }
{ i.e. kCFStringEncodingMacRoman -> kCFStringEncodingMacRoman }
{	kCFStringEncodingWindowsLatin1 -> kCFStringEncodingMacRoman }
{	kCFStringEncodingISO_2022_JP -> kCFStringEncodingMacJapanese }
function CFStringGetMostCompatibleMacStringEncoding( encoding: CFStringEncoding ): CFStringEncoding; external name '_CFStringGetMostCompatibleMacStringEncoding';


{ The next two functions allow fast access to the contents of a string, 
   assuming you are doing sequential or localized accesses. To use, call
   CFStringInitInlineBuffer() with a CFStringInlineBuffer (on the stack, say),
   and a range in the string to look at. Then call CFStringGetCharacterFromInlineBuffer()
   as many times as you want, with a index into that range (relative to the start
   of that range). These are INLINE functions and will end up calling CFString only 
   once in a while, to fill a buffer.  CFStringGetCharacterFromInlineBuffer() returns 0 if
   a location outside the original range is specified.
}
const
	__kCFStringInlineBufferLength = 64;
type
	CFStringInlineBuffer = record
		buffer: array[0..__kCFStringInlineBufferLength-1] of UniChar;
		theString: CFStringRef;
		directBuffer: UniCharPtr;
		rangeToBuffer: CFRange;		{ Range in string to buffer }
		bufferedRangeStart: CFIndex;		{ Start of range currently buffered (relative to rangeToBuffer.location) }
		bufferedRangeEnd: CFIndex;		{ bufferedRangeStart + number of chars actually buffered }
	end;

// Not currently converted to Pascal
// #if defined(CF_INLINE)
// CF_INLINE void CFStringInitInlineBuffer(CFStringRef str, CFStringInlineBuffer *buf, CFRange range) {
//     buf->theString = str;
//     buf->rangeToBuffer = range;
//     buf->directBuffer = CFStringGetCharactersPtr(str);
//     buf->bufferedRangeStart = buf->bufferedRangeEnd = 0;
// }
// 
// CF_INLINE UniChar CFStringGetCharacterFromInlineBuffer(CFStringInlineBuffer *buf, CFIndex idx) {
//     if (buf->directBuffer) {
// 	if (idx < 0 || idx >= buf->rangeToBuffer.length) return 0;
//         return buf->directBuffer[idx + buf->rangeToBuffer.location];
//     }
//     if (idx >= buf->bufferedRangeEnd || idx < buf->bufferedRangeStart) {
// 	if (idx < 0 || idx >= buf->rangeToBuffer.length) return 0;
// 	if ((buf->bufferedRangeStart = idx - 4) < 0) buf->bufferedRangeStart = 0;
// 	buf->bufferedRangeEnd = buf->bufferedRangeStart + __kCFStringInlineBufferLength;
// 	if (buf->bufferedRangeEnd > buf->rangeToBuffer.length) buf->bufferedRangeEnd = buf->rangeToBuffer.length;
// 	CFStringGetCharacters(buf->theString, CFRangeMake(buf->rangeToBuffer.location + buf->bufferedRangeStart, buf->bufferedRangeEnd - buf->bufferedRangeStart), buf->buffer);
//     }
//     return buf->buffer[idx - buf->bufferedRangeStart];
// }
// 
// #else
// { If INLINE functions are not available, we do somewhat less powerful macros that work similarly (except be aware that the buf argument is evaluated multiple times).
// }
// #define CFStringInitInlineBuffer(str, buf, range) \
//     do {(buf)->theString = str; (buf)->rangeToBuffer = range; (buf)->directBuffer = CFStringGetCharactersPtr(str);} while (0)
// 
// #define CFStringGetCharacterFromInlineBuffer(buf, idx) \
//     (((idx) < 0 || (idx) >= (buf)->rangeToBuffer.length) ? 0 : ((buf)->directBuffer ? (buf)->directBuffer[(idx) + (buf)->rangeToBuffer.location] : CFStringGetCharacterAtIndex((buf)->theString, (idx) + (buf)->rangeToBuffer.location)))
// 
// #endif { CF_INLINE }

{ Rest of the stuff in this file is private and should not be used directly
}
{ For debugging only
   Use CFShow() to printf the description of any CFType;
   Use CFShowStr() to printf detailed info about a CFString
}
procedure CFShow( obj: CFTypeRef ); external name '_CFShow';

procedure CFShowStr( str: CFStringRef ); external name '_CFShowStr';

{ This function is private and should not be used directly }
function __CFStringMakeConstantString( cStr: ConstCStringPtr ): CFStringRef; external name '___CFStringMakeConstantString';	{ Private; do not use }


implementation


end.
