{
     File:       HIToolbox/HITextUtils.h
 
     Contains:   Text utilities provided by HIToolbox.
 
     Version:    HIToolbox-219.4.81~2
 
     Copyright:  © 2002-2005 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, August 2005 }
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

unit HITextUtils;
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
uses MacTypes, TextCommon, CFBase;
{$ALIGN POWER}


{
 *  GetTextAndEncodingFromCFString()
 *  
 *  Summary:
 *    Converts the contents of a CFString to the one- or two-byte
 *    encoding that most accurately represents the original Unicode
 *    characters in the string.
 *  
 *  Discussion:
 *    Because the Macintosh toolbox has had, until Carbon, little
 *    support for Unicode, you may often find that your applications
 *    need to translate a CFString (containing Unicode characters) back
 *    to a one- or two-byte encoding of the string in order to draw,
 *    measure, or otherwise use the text in the string.
 *    GetTextAndEncodingFromCFString is a utility that may be helpful
 *    to do this, in some circumstances. The Carbon toolbox uses this
 *    API itself when translating CFStrings to Pascal strings.
 *    GetTextAndEncodingFromCFString attempts to convert the Unicode
 *    characters in the string to the closest non-Unicode
 *    representation of the string. For example, a string containing
 *    only Unicode characters that are present in the MacRoman one-byte
 *    encoding will be translated to MacRoman, and
 *    kTextEncodingMacRoman will be returned as the encoding of the
 *    text. A string containing Unicode characters that are present in
 *    the MacJapanese two-byte encoding will be translated to
 *    MacJapanese (Shift-JIS), and kTextEncodingMacJapanese will be
 *    returned as the encoding of the text.
 *    GetTextAndEncodingFromCFString is designed to be used for simple
 *    strings which contain only text from a single language. It is not
 *    designed to translate strings with multiple runs of text from
 *    different languages, and will only return the first run of such a
 *    string; for example, if you pass it a string containing a run of
 *    Roman characters followed by a run of Japanese characters, it
 *    will only return the Roman characters in the output buffer. For
 *    more complex text translation, you should call the Unicode
 *    converter directly (see ConvertFromUnicodeToTextRun, which
 *    GetTextAndEncodingFromCFString uses internally).
 *  
 *  Mac OS X threading:
 *    Not thread safe
 *  
 *  Parameters:
 *    
 *    inString:
 *      The string to convert.
 *    
 *    outText:
 *      The output buffer in which to place the converted text.
 *    
 *    inTextMaxLength:
 *      The length in bytes of the input buffer.
 *    
 *    outTextLength:
 *      On exit, contains the length in bytes of the text that was
 *      placed into the output buffer.
 *    
 *    outEncoding:
 *      On exit, contains the encoding of the text that was placed into
 *      the output buffer.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in Carbon.framework
 *    CarbonLib:        in CarbonLib 1.2.5 and later
 *    Non-Carbon CFM:   not available
 }
function GetTextAndEncodingFromCFString( inString: CFStringRef; outText: BytePtr; inTextMaxLength: ByteCount; var outTextLength: ByteCount; outEncoding: TextEncodingPtr { can be NULL } ): OSStatus; external name '_GetTextAndEncodingFromCFString';
(* AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER *)


end.
