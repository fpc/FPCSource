{
     File:       AppleScript.p
 
     Contains:   AppleScript Specific Interfaces.
 
     Version:    Technology: AppleScript 1.1
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1992-2002 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


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

unit AppleScript;
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
uses MacTypes,AEDataModel,Components,MacErrors,AppleEvents,OSA,TextEdit;


{$ALIGN MAC68K}

{*************************************************************************
    Types and Constants
*************************************************************************}
{
    The specific type for the AppleScript instance of the
    Open Scripting Architecture type.
}

const
	typeAppleScript				= $61736372 (* 'ascr' *);
	kAppleScriptSubtype			= $61736372 (* 'ascr' *);
	typeASStorage				= $61736372 (* 'ascr' *);

	{	*************************************************************************
	    Component Selectors
	*************************************************************************	}

	kASSelectInit				= $1001;
	kASSelectSetSourceStyles	= $1002;
	kASSelectGetSourceStyles	= $1003;
	kASSelectGetSourceStyleNames = $1004;


	{	*************************************************************************
	    OSAGetScriptInfo Selectors
	*************************************************************************	}
	kASHasOpenHandler			= $68736F64 (* 'hsod' *);

	{	
	        This selector is used to query a context as to whether it contains
	        a handler for the kAEOpenDocuments event. This allows "applets" to be 
	        distinguished from "droplets."  OSAGetScriptInfo returns false if
	        there is no kAEOpenDocuments handler, and returns the error value 
	        errOSAInvalidAccess if the input is not a context.
	    	}
	{	*************************************************************************
	    Initialization
	*************************************************************************	}
	{
	 *  ASInit()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function ASInit(scriptingComponent: ComponentInstance; modeFlags: SInt32; minStackSize: SInt32; preferredStackSize: SInt32; maxStackSize: SInt32; minHeapSize: SInt32; preferredHeapSize: SInt32; maxHeapSize: SInt32): OSAError; external name '_ASInit';
{
        ComponentCallNow(kASSelectInit, 28);
        This call can be used to explicitly initialize AppleScript.  If it is
        not called, the a scripting size resource is looked for and used. If
        there is no scripting size resource, then the constants listed below
        are used.  If at any stage (the init call, the size resource, the 
        defaults) any of these parameters are zero, then parameters from the
        next stage are used.  ModeFlags are not currently used.
        Errors:
        errOSASystemError       initialization failed
    }
{
    These values will be used if ASInit is not called explicitly, or if any
    of ASInit's parameters are zero:
}

const
	kASDefaultMinStackSize		= 4096;
	kASDefaultPreferredStackSize = 16384;
	kASDefaultMaxStackSize		= 16384;
	kASDefaultMinHeapSize		= 4096;
	kASDefaultPreferredHeapSize	= 16384;
	kASDefaultMaxHeapSize		= 33554432;

	{	*************************************************************************
	    Source Styles
	*************************************************************************	}
	{
	 *  ASSetSourceStyles()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function ASSetSourceStyles(scriptingComponent: ComponentInstance; sourceStyles: STHandle): OSAError; external name '_ASSetSourceStyles';
{
        ComponentCallNow(kASSelectSetSourceStyles, 4);
        Errors:
        errOSASystemError       operation failed
    }
{
 *  ASGetSourceStyles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ASGetSourceStyles(scriptingComponent: ComponentInstance; var resultingSourceStyles: STHandle): OSAError; external name '_ASGetSourceStyles';
{
        ComponentCallNow(kASSelectGetSourceStyles, 4);
        Errors:
        errOSASystemError       operation failed
    }
{
 *  ASGetSourceStyleNames()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ASGetSourceStyleNames(scriptingComponent: ComponentInstance; modeFlags: SInt32; var resultingSourceStyleNamesList: AEDescList): OSAError; external name '_ASGetSourceStyleNames';
{
        ComponentCallNow(kASSelectGetSourceStyleNames, 8);
        This call returns an AEList of styled text descriptors the names of the
        source styles in the current dialect.  The order of the names corresponds
        to the order of the source style constants, below.  The style of each
        name is the same as the styles returned by ASGetSourceStyles.
        
        Errors:
        errOSASystemError       operation failed
    }
{
    Elements of STHandle correspond to following categories of tokens, and
    accessed through following index constants:
}

const
	kASSourceStyleUncompiledText = 0;
	kASSourceStyleNormalText	= 1;
	kASSourceStyleLanguageKeyword = 2;
	kASSourceStyleApplicationKeyword = 3;
	kASSourceStyleComment		= 4;
	kASSourceStyleLiteral		= 5;
	kASSourceStyleUserSymbol	= 6;
	kASSourceStyleObjectSpecifier = 7;
	kASNumberOfSourceStyles		= 8;


{$ALIGN MAC68K}


end.
