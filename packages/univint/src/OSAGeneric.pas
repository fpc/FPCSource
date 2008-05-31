{
     File:       OSAGeneric.p
 
     Contains:   AppleScript Generic Component Interfaces.
 
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

unit OSAGeneric;
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
uses MacTypes,AEDataModel,Components,MacErrors,AppleEvents,OSA;


{$ALIGN MAC68K}

{    NOTE:   This interface defines a "generic scripting component."
            The Generic Scripting Component allows automatic dispatch to a
            specific scripting component that conforms to the OSA interface.
            This component supports OSA, by calling AppleScript or some other 
            scripting component.  Additionally it provides access to the default
            and the user-prefered scripting component.
}


const
																{  Component version this header file describes  }
	kGenericComponentVersion	= $0100;

	kGSSSelectGetDefaultScriptingComponent = $1001;
	kGSSSelectSetDefaultScriptingComponent = $1002;
	kGSSSelectGetScriptingComponent = $1003;
	kGSSSelectGetScriptingComponentFromStored = $1004;
	kGSSSelectGenericToRealID	= $1005;
	kGSSSelectRealToGenericID	= $1006;
	kGSSSelectOutOfRange		= $1007;


type
	ScriptingComponentSelector			= OSType;
	GenericID							= OSAID;
	{	 get and set the default scripting component 	}
	{
	 *  OSAGetDefaultScriptingComponent()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function OSAGetDefaultScriptingComponent(genericScriptingComponent: ComponentInstance; var scriptingSubType: ScriptingComponentSelector): OSAError; external name '_OSAGetDefaultScriptingComponent';
{
 *  OSASetDefaultScriptingComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSASetDefaultScriptingComponent(genericScriptingComponent: ComponentInstance; scriptingSubType: ScriptingComponentSelector): OSAError; external name '_OSASetDefaultScriptingComponent';
{ get a scripting component instance from its subtype code }
{
 *  OSAGetScriptingComponent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetScriptingComponent(genericScriptingComponent: ComponentInstance; scriptingSubType: ScriptingComponentSelector; var scriptingInstance: ComponentInstance): OSAError; external name '_OSAGetScriptingComponent';
{ get a scripting component selector (subType) from a stored script }
{
 *  OSAGetScriptingComponentFromStored()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGetScriptingComponentFromStored(genericScriptingComponent: ComponentInstance; const (*var*) scriptData: AEDesc; var scriptingSubType: ScriptingComponentSelector): OSAError; external name '_OSAGetScriptingComponentFromStored';
{ get a real component instance and script id from a generic id }
{
 *  OSAGenericToRealID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSAGenericToRealID(genericScriptingComponent: ComponentInstance; var theScriptID: OSAID; var theExactComponent: ComponentInstance): OSAError; external name '_OSAGenericToRealID';
{ get a generic id from a real component instance and script id }
{
 *  OSARealToGenericID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in AppleScriptLib 1.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function OSARealToGenericID(genericScriptingComponent: ComponentInstance; var theScriptID: OSAID; theExactComponent: ComponentInstance): OSAError; external name '_OSARealToGenericID';
{$ALIGN MAC68K}


end.
