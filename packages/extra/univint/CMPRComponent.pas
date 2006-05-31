{
     File:       CMPRComponent.p
 
     Contains:   ColorSync ProfileResponder Component API
 
     Version:    Technology: ColorSync 1.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1993-2002 by Apple Computer, Inc. All rights reserved.
 
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

unit CMPRComponent;
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
uses MacTypes,CMTypes,CMICCProfile,Quickdraw,Components,CMApplication;


{$ALIGN MAC68K}


const
	CMPRInterfaceVersion		= 0;

	{	 Component function selectors 	}
	kCMPRGetProfile				= 0;
	kCMPRSetProfile				= 1;
	kCMPRSetProfileDescription	= 2;
	kCMPRGetIndexedProfile		= 3;
	kCMPRDeleteDeviceProfile	= 4;


{$ifc CALL_NOT_IN_CARBON}
	{
	 *  CMGetProfile()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in ColorSyncLibPriv 2.0 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function CMGetProfile(pr: ComponentInstance; aProfile: CMProfileHandle; var returnedProfile: CMProfileHandle): CMError; external name '_CMGetProfile';
{
 *  CMSetProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLibPriv 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMSetProfile(pr: ComponentInstance; newProfile: CMProfileHandle): CMError; external name '_CMSetProfile';
{
 *  CMSetProfileDescription()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLibPriv 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMSetProfileDescription(pr: ComponentInstance; DeviceData: SInt32; hProfile: CMProfileHandle): CMError; external name '_CMSetProfileDescription';
{
 *  CMGetIndexedProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLibPriv 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMGetIndexedProfile(pr: ComponentInstance; search: CMProfileSearchRecordHandle; var returnProfile: CMProfileHandle; var index: SInt32): CMError; external name '_CMGetIndexedProfile';
{
 *  CMDeleteDeviceProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ColorSyncLibPriv 2.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function CMDeleteDeviceProfile(pr: ComponentInstance; deleteMe: CMProfileHandle): CMError; external name '_CMDeleteDeviceProfile';
{$endc}  {CALL_NOT_IN_CARBON}

{$ALIGN MAC68K}


end.
