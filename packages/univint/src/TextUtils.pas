{
     File:       TextUtils.p
 
     Contains:   Text Utilities Interfaces.
 
     Version:    Technology: Mac OS 8
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1985-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit TextUtils;
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
uses MacTypes,IntlResources,NumberFormatting,StringCompare,DateTimeUtils;


{$ALIGN MAC68K}

{

    Here are the current System 7 routine names and the translations to the older forms.
    Please use the newer forms in all new code and migrate the older names out of existing
    code as maintainance permits.
    
    NEW NAME                    OLD NAMEs                   OBSOLETE FORM (no script code)

    FindScriptRun
    FindWordBreaks                                          NFindWord, FindWord
    GetIndString            
    GetString
    Munger
    NewString               
    SetString               
    StyledLineBreak
    TruncString
    TruncText

    UpperString ($A054)         UprString, UprText
    UppercaseText               SCUpperText (a only)        UpperText ($A456)
    LowercaseText                                           LwrString, LowerText, LwrText ($A056)
    StripDiacritics                                         StripText ($A256)
    UppercaseStripDiacritics                                StripUpperText ($A656)


}

{ TruncCode, StyledLineBreakCode, and truncation constants moved to QuickDrawText.i }

type
	ScriptRunStatusPtr = ^ScriptRunStatus;
	ScriptRunStatus = record
		script:					SInt8;
		runVariant:				SInt8;
	end;

	BreakTablePtr = ^BreakTable;
	BreakTable = record
		charTypes:				packed array [0..255] of char;
		tripleLength:			SInt16;
		triples:				array [0..0] of SInt16;
	end;

	NBreakTablePtr = ^NBreakTable;
	NBreakTable = record
		flags1:					SInt8;
		flags2:					SInt8;
		version:				SInt16;
		classTableOff:			SInt16;
		auxCTableOff:			SInt16;
		backwdTableOff:			SInt16;
		forwdTableOff:			SInt16;
		doBackup:				SInt16;
		length:					SInt16;								{  length of NBreakTable  }
		charTypes:				packed array [0..255] of char;
		tables:					array [0..0] of SInt16;
	end;

	{  The following functions are new names that work on 68k and PowerPC }
	{
	 *  Munger()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function Munger(h: Handle; offset: SInt32; ptr1: UnivPtr; len1: SInt32; ptr2: UnivPtr; len2: SInt32): SInt32; external name '_Munger';
{
 *  NewString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewString(const (*var*) theString: Str255): StringHandle; external name '_NewString';
{
 *  SetString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetString(theString: StringHandle; const (*var*) strNew: Str255); external name '_SetString';
{
 *  GetString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetString(stringID: SInt16): StringHandle; external name '_GetString';
{
 *  GetIndString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetIndString(var theString: Str255; strListID: SInt16; index: SInt16); external name '_GetIndString';

{
 *  FindWordBreaks()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure FindWordBreaks(textPtr: Ptr; textLength: SInt16; offset: SInt16; leadingEdge: boolean; breaks: BreakTablePtr; var offsets: OffsetTable; script: ScriptCode); external name '_FindWordBreaks';
{
 *  LowercaseText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LowercaseText(textPtr: Ptr; len: SInt16; script: ScriptCode); external name '_LowercaseText';
{
 *  UppercaseText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure UppercaseText(textPtr: Ptr; len: SInt16; script: ScriptCode); external name '_UppercaseText';
{
 *  StripDiacritics()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure StripDiacritics(textPtr: Ptr; len: SInt16; script: ScriptCode); external name '_StripDiacritics';
{
 *  UppercaseStripDiacritics()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure UppercaseStripDiacritics(textPtr: Ptr; len: SInt16; script: ScriptCode); external name '_UppercaseStripDiacritics';
{
 *  FindScriptRun()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FindScriptRun(textPtr: Ptr; textLen: SInt32; var lenUsed: SInt32): ScriptRunStatus; external name '_FindScriptRun';
{
    The following functions are old names, but are required for PowerPC builds
    because InterfaceLib exports these names, instead of the new ones.
}

{$ifc CALL_NOT_IN_CARBON}
{
 *  FindWord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure FindWord(textPtr: Ptr; textLength: SInt16; offset: SInt16; leadingEdge: boolean; breaks: BreakTablePtr; var offsets: OffsetTable); external name '_FindWord';
{
 *  NFindWord()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure NFindWord(textPtr: Ptr; textLength: SInt16; offset: SInt16; leadingEdge: boolean; nbreaks: NBreakTablePtr; var offsets: OffsetTable); external name '_NFindWord';
{
   On 68K machines, LwrText, LowerText, StripText, UpperText and StripUpperText
   return an error code in register D0, but System 7 PowerMacs do not emulate
   this properly, so checking D0 is unreliable.
}

{
 *  LwrText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LwrText(textPtr: Ptr; len: SInt16); external name '_LwrText';
{
 *  LowerText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LowerText(textPtr: Ptr; len: SInt16); external name '_LowerText';
{
 *  StripText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure StripText(textPtr: Ptr; len: SInt16); external name '_StripText';
{
 *  UpperText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure UpperText(textPtr: Ptr; len: SInt16); external name '_UpperText';
{
 *  StripUpperText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure StripUpperText(textPtr: Ptr; len: SInt16); external name '_StripUpperText';
{  The following are new names which are exported by InterfaceLib }

{$endc}  {CALL_NOT_IN_CARBON}

{
 *  UpperString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure UpperString(var theString: Str255; diacSensitive: boolean); external name '_UpperString';

{  Old routine name but no new names are mapped to it: }
{$ifc CALL_NOT_IN_CARBON}
{
 *  UprText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure UprText(textPtr: Ptr; len: SInt16); external name '_UprText';
{$endc}  {CALL_NOT_IN_CARBON}

{
    Functions for converting between C and Pascal Strings
    (Previously in Strings.h)
    
    Note: CopyPascalStringToC, CopyCStringToPascal, c2pstrcpy, and p2cstrcpy
          are written to allow inplace conversion.  That is, the src and dst
          parameters can point to the memory location.  These functions
          are available in CarbonLib and CarbonAccessors.o.
          
    Note: c2pstr, C2PStr, p2cstr, and P2CStr are all deprecated.  These functions
          only do inplace conversion and often require casts to call them.  This can
          cause bugs because you can easily cast away a const and change the 
          contents of a read-only buffer.  These functions are available
          in InterfaceLib, or when building for Carbon if you #define OLDP2C,
          then they are available as a macro.
    
}
{
 *  c2pstrcpy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CarbonAccessors.o 1.0.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure c2pstrcpy(var dst: Str255; src: ConstCStringPtr); external name '_c2pstrcpy';

{
 *  p2cstrcpy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CarbonAccessors.o 1.0.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure p2cstrcpy(dst: CStringPtr; const (*var*) src: Str255); external name '_p2cstrcpy';

{
 *  CopyPascalStringToC()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CarbonAccessors.o 1.0.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure CopyPascalStringToC(const (*var*) src: Str255; dst: CStringPtr); external name '_CopyPascalStringToC';

{
 *  CopyCStringToPascal()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CarbonAccessors.o 1.0.2 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure CopyCStringToPascal(src: ConstCStringPtr; var dst: Str255); external name '_CopyCStringToPascal';

{$ifc CALL_NOT_IN_CARBON}
{
 *  C2PStrProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure C2PStrProc(aStr: UnivPtr); external name '_C2PStrProc';

{
 *  P2CStrProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure P2CStrProc(aStr: StringPtr); external name '_P2CStrProc';

{
 *  C2PStr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function C2PStr(cString: UnivPtr): StringPtr; external name '_C2PStr';

{
 *  P2CStr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function P2CStr(pString: StringPtr): Ptr; external name '_P2CStr';

{$endc}  {CALL_NOT_IN_CARBON}


{$ALIGN MAC68K}


end.
