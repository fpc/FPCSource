{
     File:       NumberFormatting.p
 
     Contains:   Utilites for formatting numbers
 
     Version:    Universal Interfaces 3.4.2
 
     Copyright:  © 1996-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


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

unit NumberFormatting;
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
uses MacTypes,ConditionalMacros,IntlResources;


{$ALIGN MAC68K}

{

    Here are the current System 7 routine names and the translations to the older forms.
    Please use the newer forms in all new code and migrate the older names out of existing
    code as maintainance permits.
    
    New Name                    Old Name(s)
    
    ExtendedToString            FormatX2Str
    FormatRecToString           Format2Str
    NumToString             
    StringToExtended            FormatStr2X
    StringToFormatRec           Str2Format
    StringToNum             

}

type
	NumFormatStringPtr = ^NumFormatString;
	NumFormatString = packed record
		fLength:				UInt8;
		fVersion:				UInt8;
		data:					packed array [0..253] of char;			{  private data  }
	end;

	NumFormatStringRec					= NumFormatString;
	NumFormatStringRecPtr 				= ^NumFormatStringRec;
	FormatStatus						= SInt16;

const
	fVNumber					= 0;							{  first version of NumFormatString  }


type
	FormatClass							= SInt8;

const
	fPositive					= 0;
	fNegative					= 1;
	fZero						= 2;


type
	FormatResultType					= SInt8;

const
	fFormatOK					= 0;
	fBestGuess					= 1;
	fOutOfSynch					= 2;
	fSpuriousChars				= 3;
	fMissingDelimiter			= 4;
	fExtraDecimal				= 5;
	fMissingLiteral				= 6;
	fExtraExp					= 7;
	fFormatOverflow				= 8;
	fFormStrIsNAN				= 9;
	fBadPartsTable				= 10;
	fExtraPercent				= 11;
	fExtraSeparator				= 12;
	fEmptyFormatString			= 13;


type
	FVectorPtr = ^FVector;
	FVector = record
		start:					SInt16;
		length:					SInt16;
	end;

	{	 index by [fPositive..fZero] 	}
	TripleInt							= array [0..2] of FVector;
	{
	 *  StringToNum()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure StringToNum(const (*var*) theString: Str255; var theNum: SInt32); external name '_StringToNum';

{
 *  NumToString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure NumToString(theNum: SInt32; var theString: Str255); external name '_NumToString';

{
 *  ExtendedToString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ExtendedToString(const (*var*) x: extended80; const (*var*) myCanonical: NumFormatString; const (*var*) partsTable: NumberParts; var outString: Str255): FormatStatus; external name '_ExtendedToString';
{
 *  StringToExtended()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StringToExtended(const (*var*) source: Str255; const (*var*) myCanonical: NumFormatString; const (*var*) partsTable: NumberParts; var x: extended80): FormatStatus; external name '_StringToExtended';
{
 *  StringToFormatRec()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StringToFormatRec(const (*var*) inString: Str255; const (*var*) partsTable: NumberParts; var outString: NumFormatString): FormatStatus; external name '_StringToFormatRec';
{
 *  FormatRecToString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FormatRecToString(const (*var*) myCanonical: NumFormatString; const (*var*) partsTable: NumberParts; var outString: Str255; var positions: TripleInt): FormatStatus; external name '_FormatRecToString';
{$ifc OLDROUTINENAMES}
{$endc}  {OLDROUTINENAMES}

{$ALIGN MAC68K}


end.
