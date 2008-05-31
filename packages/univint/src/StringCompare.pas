{
     File:       StringCompare.p
 
     Contains:   Public interfaces for String Comparison and related operations
 
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

unit StringCompare;
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
uses MacTypes,MixedMode,TextCommon,Script,TypeSelect;

{$ALIGN MAC68K}

{

    Here are the current System 7 routine names and the translations to the older forms.
    Please use the newer forms in all new code and migrate the older names out of existing
    code as maintenance permits.
    
    NEW NAME                    OLD NAME                    OBSOLETE FORM (no handle)
    
    CompareString (Str255)      IUCompPString (hp only)     IUCompString (hp only)
    CompareText (ptr/len)       IUMagPString                IUMagString
    IdenticalString (Str255)    IUEqualPString (hp only)    IUEqualString  (hp only)
    IdenticalText (ptr/len)     IUMagIDPString              IUMagIDString
    LanguageOrder               IULangOrder
    ScriptOrder                 IUScriptOrder
    StringOrder (Str255)        IUStringOrder (hp only)
    TextOrder (ptr/len)         IUTextOrder

    RelString
    CmpString (a only)                  
    EqualString (hp only)
    
    ReplaceText

    Carbon only supports the new names.  The old names are undefined for Carbon targets.

    InterfaceLib always has exported the old names.  For C macros have been defined to allow
    the use of the new names.  For Pascal and Assembly using the new names will result
    in link errors. 
    
}


const
																{  Special language code values for Language Order }
	systemCurLang				= -2;							{  current (itlbLang) lang for system script }
	systemDefLang				= -3;							{  default (table) lang for system script }
	currentCurLang				= -4;							{  current (itlbLang) lang for current script }
	currentDefLang				= -5;							{  default lang for current script }
	scriptCurLang				= -6;							{  current (itlbLang) lang for specified script }
	scriptDefLang				= -7;							{  default language for a specified script }

	{  obsolete names }
	iuSystemCurLang				= -2;
	iuSystemDefLang				= -3;
	iuCurrentCurLang			= -4;
	iuCurrentDefLang			= -5;
	iuScriptCurLang				= -6;
	iuScriptDefLang				= -7;


	{	
	 *  These routines are available in Carbon with the new names.
	 	}
	{
	 *  [Mac]ReplaceText()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function ReplaceText(baseText: Handle; substitutionText: Handle; var key: Str15): SInt16; external name '_ReplaceText';
{
 *  ScriptOrder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ScriptOrder(script1: ScriptCode; script2: ScriptCode): SInt16; external name '_ScriptOrder';
{
 *  [Mac]CompareString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CompareString(const (*var*) aStr: Str255; const (*var*) bStr: Str255; itl2Handle: Handle): SInt16; external name '_CompareString';

{
 *  IdenticalString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IdenticalString(const (*var*) aStr: Str255; const (*var*) bStr: Str255; itl2Handle: Handle): SInt16; external name '_IdenticalString';

{
 *  StringOrder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StringOrder(const (*var*) aStr: Str255; const (*var*) bStr: Str255; aScript: ScriptCode; bScript: ScriptCode; aLang: LangCode; bLang: LangCode): SInt16; external name '_StringOrder';

{
 *  CompareText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CompareText(aPtr: UnivPtr; bPtr: UnivPtr; aLen: SInt16; bLen: SInt16; itl2Handle: Handle): SInt16; external name '_CompareText';
{
 *  IdenticalText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IdenticalText(aPtr: UnivPtr; bPtr: UnivPtr; aLen: SInt16; bLen: SInt16; itl2Handle: Handle): SInt16; external name '_IdenticalText';
{
 *  TextOrder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TextOrder(aPtr: UnivPtr; bPtr: UnivPtr; aLen: SInt16; bLen: SInt16; aScript: ScriptCode; bScript: ScriptCode; aLang: LangCode; bLang: LangCode): SInt16; external name '_TextOrder';
{
 *  LanguageOrder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LanguageOrder(language1: LangCode; language2: LangCode): SInt16; external name '_LanguageOrder';
{
 *  These routines are available in InterfaceLib with old names.
 *  Macros are provided for C to allow source code use to the new names.
 }
{$ifc CALL_NOT_IN_CARBON}
{
 *  IUMagPString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IUMagPString(aPtr: UnivPtr; bPtr: UnivPtr; aLen: SInt16; bLen: SInt16; itl2Handle: Handle): SInt16; external name '_IUMagPString';
{
 *  IUMagIDPString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IUMagIDPString(aPtr: UnivPtr; bPtr: UnivPtr; aLen: SInt16; bLen: SInt16; itl2Handle: Handle): SInt16; external name '_IUMagIDPString';
{
 *  IUTextOrder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IUTextOrder(aPtr: UnivPtr; bPtr: UnivPtr; aLen: SInt16; bLen: SInt16; aScript: ScriptCode; bScript: ScriptCode; aLang: LangCode; bLang: LangCode): SInt16; external name '_IUTextOrder';
{
 *  IULangOrder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IULangOrder(language1: LangCode; language2: LangCode): SInt16; external name '_IULangOrder';
{
 *  IUScriptOrder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IUScriptOrder(script1: ScriptCode; script2: ScriptCode): SInt16; external name '_IUScriptOrder';
{
 *  IUMagString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IUMagString(aPtr: UnivPtr; bPtr: UnivPtr; aLen: SInt16; bLen: SInt16): SInt16; external name '_IUMagString';
{
 *  IUMagIDString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IUMagIDString(aPtr: UnivPtr; bPtr: UnivPtr; aLen: SInt16; bLen: SInt16): SInt16; external name '_IUMagIDString';
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc CALL_NOT_IN_CARBON}
{
 *  IUCompPString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IUCompPString(const (*var*) aStr: Str255; const (*var*) bStr: Str255; itl2Handle: Handle): SInt16; external name '_IUCompPString';

{
 *  IUEqualPString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IUEqualPString(const (*var*) aStr: Str255; const (*var*) bStr: Str255; itl2Handle: Handle): SInt16; external name '_IUEqualPString';

{
 *  IUStringOrder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IUStringOrder(const (*var*) aStr: Str255; const (*var*) bStr: Str255; aScript: ScriptCode; bScript: ScriptCode; aLang: LangCode; bLang: LangCode): SInt16; external name '_IUStringOrder';

{
 *  IUCompString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IUCompString(const (*var*) aStr: Str255; const (*var*) bStr: Str255): SInt16; external name '_IUCompString';

{
 *  IUEqualString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function IUEqualString(const (*var*) aStr: Str255; const (*var*) bStr: Str255): SInt16; external name '_IUEqualString';

{$endc}  {CALL_NOT_IN_CARBON}


{
 *  RelString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RelString(const (*var*) str1: Str255; const (*var*) str2: Str255; caseSensitive: boolean; diacSensitive: boolean): SInt16; external name '_RelString';

{
 *  EqualString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function EqualString(const (*var*) str1: Str255; const (*var*) str2: Str255; caseSensitive: boolean; diacSensitive: boolean): boolean; external name '_EqualString';


{$ALIGN MAC68K}


end.
