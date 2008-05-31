{
     File:       ASDebugging.p
 
     Contains:   AppleScript Debugging Interfaces.
 
     Version:    Technology: AppleScript 1.1
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1992-2002 by Apple Computer, Inc., all rights reserved
 
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

unit ASDebugging;
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
uses MacTypes,AEDataModel,OSA,Files,Components,AppleEvents,AppleScript;


{$ALIGN MAC68K}

{*************************************************************************
    Mode Flags
*************************************************************************}
{    This mode flag can be passed to OSASetProperty or OSASetHandler
    and will prevent properties or handlers from being defined in a context
    that doesn't already have bindings for them. An error is returned if
    a current binding doesn't already exist. 
}

const
	kOSAModeDontDefine			= $0001;

	{	*************************************************************************
	    Component Selectors
	*************************************************************************	}
	kASSelectSetPropertyObsolete = $1101;
	kASSelectGetPropertyObsolete = $1102;
	kASSelectSetHandlerObsolete	= $1103;
	kASSelectGetHandlerObsolete	= $1104;
	kASSelectGetAppTerminologyObsolete = $1105;
	kASSelectSetProperty		= $1106;
	kASSelectGetProperty		= $1107;
	kASSelectSetHandler			= $1108;
	kASSelectGetHandler			= $1109;
	kASSelectGetAppTerminology	= $110A;
	kASSelectGetSysTerminology	= $110B;
	kASSelectGetPropertyNames	= $110C;
	kASSelectGetHandlerNames	= $110D;

	{	*************************************************************************
	    Context Accessors
	*************************************************************************	}
	{
	 *  OSASetProperty()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function OSASetProperty(scriptingComponent: ComponentInstance; modeFlags: SInt32; contextID: OSAID; const (*var*) variableName: AEDesc; scriptValueID: OSAID): OSAError; external name '_OSASetProperty';
{
 *  OSAGetProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetProperty(scriptingComponent: ComponentInstance; modeFlags: SInt32; contextID: OSAID; const (*var*) variableName: AEDesc; var resultingScriptValueID: OSAID): OSAError; external name '_OSAGetProperty';
{
 *  OSAGetPropertyNames()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetPropertyNames(scriptingComponent: ComponentInstance; modeFlags: SInt32; contextID: OSAID; var resultingPropertyNames: AEDescList): OSAError; external name '_OSAGetPropertyNames';
{
 *  OSASetHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSASetHandler(scriptingComponent: ComponentInstance; modeFlags: SInt32; contextID: OSAID; const (*var*) handlerName: AEDesc; compiledScriptID: OSAID): OSAError; external name '_OSASetHandler';
{
 *  OSAGetHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetHandler(scriptingComponent: ComponentInstance; modeFlags: SInt32; contextID: OSAID; const (*var*) handlerName: AEDesc; var resultingCompiledScriptID: OSAID): OSAError; external name '_OSAGetHandler';
{
 *  OSAGetHandlerNames()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetHandlerNames(scriptingComponent: ComponentInstance; modeFlags: SInt32; contextID: OSAID; var resultingHandlerNames: AEDescList): OSAError; external name '_OSAGetHandlerNames';
{
 *  OSAGetAppTerminology()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetAppTerminology(scriptingComponent: ComponentInstance; modeFlags: SInt32; var fileSpec: FSSpec; terminologyID: SInt16; var didLaunch: boolean; var terminologyList: AEDesc): OSAError; external name '_OSAGetAppTerminology';
{ Errors:
       errOSASystemError        operation failed
    }
{
 *  OSAGetSysTerminology()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetSysTerminology(scriptingComponent: ComponentInstance; modeFlags: SInt32; terminologyID: SInt16; var terminologyList: AEDesc): OSAError; external name '_OSAGetSysTerminology';
{ Errors:
       errOSASystemError        operation failed
    }
{ Notes on terminology ID

    A terminology ID is derived from script code and language code
    as follows;

        terminologyID = ((scriptCode & 0x7F) << 8) | (langCode & 0xFF)
}
{*************************************************************************
    Obsolete versions provided for backward compatibility:
}
{
 *  ASSetProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ASSetProperty(scriptingComponent: ComponentInstance; contextID: OSAID; const (*var*) variableName: AEDesc; scriptValueID: OSAID): OSAError; external name '_ASSetProperty';
{
 *  ASGetProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ASGetProperty(scriptingComponent: ComponentInstance; contextID: OSAID; const (*var*) variableName: AEDesc; var resultingScriptValueID: OSAID): OSAError; external name '_ASGetProperty';
{
 *  ASSetHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ASSetHandler(scriptingComponent: ComponentInstance; contextID: OSAID; const (*var*) handlerName: AEDesc; compiledScriptID: OSAID): OSAError; external name '_ASSetHandler';
{
 *  ASGetHandler()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ASGetHandler(scriptingComponent: ComponentInstance; contextID: OSAID; const (*var*) handlerName: AEDesc; var resultingCompiledScriptID: OSAID): OSAError; external name '_ASGetHandler';
{
 *  ASGetAppTerminology()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ASGetAppTerminology(scriptingComponent: ComponentInstance; var fileSpec: FSSpec; terminologID: SInt16; var didLaunch: boolean; var terminologyList: AEDesc): OSAError; external name '_ASGetAppTerminology';
{ Errors:
        errOSASystemError       operation failed
    }
{************************************************************************}


{$ALIGN MAC68K}


end.
