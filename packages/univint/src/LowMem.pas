{
     File:       LowMem.p
 
     Contains:   Low Memory Accessor Interfaces.
 
     Version:    Technology: Mac OS 8.1
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1993-2002 by Apple Computer, Inc., all rights reserved
 
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

unit LowMem;
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
uses MacTypes,Files,MacMemory,OSUtils,Resources,Quickdraw,Controls,Events,Menus,Fonts,MacWindows;


{$ifc NOT OPAQUE_TOOLBOX_STRUCTS}
{$endc}


{$ALIGN MAC68K}

{*************************************************************************************

    SIMPLE LOWMEM ACCESSORS

*************************************************************************************}
{
    The following functions were moved to Quickdraw.h:
    
        LMSetDeviceList
        LMSetLastSPExtra
        LMGetWidthListHand
        LMSetWidthListHand
        LMGetScrHRes
        LMSetScrHRes
        LMSetScrVRes
        LMGetScrVRes
        LMGetHiliteMode
        LMSetHiliteMode
        LMGetMainDevice
        LMSetMainDevice
        LMGetDeviceList
        LMGetQDColors
        LMSetQDColors
        LMGetWidthPtr
        LMSetWidthPtr
        LMGetWidthTabHandle
        LMSetWidthTabHandle
        LMGetLastSPExtra
        LMGetLastFOND
        LMSetLastFOND
        LMGetFractEnable
        LMSetFractEnable
        LMGetTheGDevice
        LMSetTheGDevice
        LMGetCursorNew
        LMSetCursorNew
        LMGetHiliteRGB
        LMSetHiliteRGB
    
    The following functions were moved to TextEdit.h:
    
        LMGetWordRedraw
        LMSetWordRedraw

    The following functions were moved to Menus.h:
    
        LMGetTheMenu
    
    The following functions were moved to Events.h:
    
        LMGetKeyRepThresh
        LMSetKeyRepThresh
        LMGetKeyThresh
        LMSetKeyRepThresh
        LMGetKbdLast
        LMSetKbdLast
        LMGetKbdType
        LMSetKbdType

}

{
 *  LMGetMemTop()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetMemTop: Ptr; external name '_LMGetMemTop';
{
 *  LMSetMemTop()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetMemTop(value: Ptr); external name '_LMSetMemTop';
{
 *  LMGetBufPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetBufPtr: Ptr; external name '_LMGetBufPtr';
{
 *  LMSetBufPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetBufPtr(value: Ptr); external name '_LMSetBufPtr';
{
 *  LMGetHeapEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetHeapEnd: Ptr; external name '_LMGetHeapEnd';
{
 *  LMSetHeapEnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetHeapEnd(value: Ptr); external name '_LMSetHeapEnd';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetTheZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTheZone: THz; external name '_LMGetTheZone';
{
 *  LMSetTheZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTheZone(value: THz); external name '_LMSetTheZone';
{
 *  LMGetUTableBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetUTableBase: Ptr; external name '_LMGetUTableBase';
{
 *  LMSetUTableBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetUTableBase(value: Ptr); external name '_LMSetUTableBase';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetCPUFlag()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetCPUFlag: ByteParameter; external name '_LMGetCPUFlag';
{
 *  LMSetCPUFlag()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetCPUFlag(value: ByteParameter); external name '_LMSetCPUFlag';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetApplLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetApplLimit: Ptr; external name '_LMGetApplLimit';
{
 *  LMSetApplLimit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetApplLimit(value: Ptr); external name '_LMSetApplLimit';
{
 *  LMGetSysEvtMask()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSysEvtMask: SInt16; external name '_LMGetSysEvtMask';
{  Carbon Usage: use SetEventMask }
{
 *  LMSetSysEvtMask()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSysEvtMask(value: SInt16); external name '_LMSetSysEvtMask';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetRndSeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetRndSeed: SInt32; external name '_LMGetRndSeed';
{
 *  LMSetRndSeed()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetRndSeed(value: SInt32); external name '_LMSetRndSeed';
{
 *  LMGetSEvtEnb()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetSEvtEnb: ByteParameter; external name '_LMGetSEvtEnb';
{
 *  LMSetSEvtEnb()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetSEvtEnb(value: ByteParameter); external name '_LMSetSEvtEnb';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetTicks()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTicks: UInt32; external name '_LMGetTicks';
{
 *  LMSetTicks()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTicks(value: UInt32); external name '_LMSetTicks';
{
 *  LMGetVIA()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetVIA: Ptr; external name '_LMGetVIA';
{
 *  LMSetVIA()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetVIA(value: Ptr); external name '_LMSetVIA';
{
 *  LMGetSCCRd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSCCRd: Ptr; external name '_LMGetSCCRd';
{
 *  LMSetSCCRd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSCCRd(value: Ptr); external name '_LMSetSCCRd';
{
 *  LMGetSCCWr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSCCWr: Ptr; external name '_LMGetSCCWr';
{
 *  LMSetSCCWr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSCCWr(value: Ptr); external name '_LMSetSCCWr';
{
 *  LMGetSPValid()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPValid: ByteParameter; external name '_LMGetSPValid';
{
 *  LMSetSPValid()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPValid(value: ByteParameter); external name '_LMSetSPValid';
{
 *  LMGetSPATalkA()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPATalkA: ByteParameter; external name '_LMGetSPATalkA';
{
 *  LMSetSPATalkA()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPATalkA(value: ByteParameter); external name '_LMSetSPATalkA';
{
 *  LMGetSPATalkB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPATalkB: ByteParameter; external name '_LMGetSPATalkB';
{
 *  LMSetSPATalkB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPATalkB(value: ByteParameter); external name '_LMSetSPATalkB';
{
 *  LMGetSPConfig()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPConfig: ByteParameter; external name '_LMGetSPConfig';
{
 *  LMSetSPConfig()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPConfig(value: ByteParameter); external name '_LMSetSPConfig';
{
 *  LMGetSPPortA()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPPortA: SInt16; external name '_LMGetSPPortA';
{
 *  LMSetSPPortA()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPPortA(value: SInt16); external name '_LMSetSPPortA';
{
 *  LMGetSPPortB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPPortB: SInt16; external name '_LMGetSPPortB';
{
 *  LMSetSPPortB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPPortB(value: SInt16); external name '_LMSetSPPortB';
{
 *  LMGetSPAlarm()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPAlarm: SInt32; external name '_LMGetSPAlarm';
{
 *  LMSetSPAlarm()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPAlarm(value: SInt32); external name '_LMSetSPAlarm';
{
 *  LMGetSPFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPFont: SInt16; external name '_LMGetSPFont';
{
 *  LMSetSPFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPFont(value: SInt16); external name '_LMSetSPFont';
{
 *  LMGetSPKbd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPKbd: ByteParameter; external name '_LMGetSPKbd';
{
 *  LMSetSPKbd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPKbd(value: ByteParameter); external name '_LMSetSPKbd';
{
 *  LMGetSPPrint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPPrint: ByteParameter; external name '_LMGetSPPrint';
{
 *  LMSetSPPrint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPPrint(value: ByteParameter); external name '_LMSetSPPrint';
{
 *  LMGetSPVolCtl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPVolCtl: ByteParameter; external name '_LMGetSPVolCtl';
{
 *  LMSetSPVolCtl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPVolCtl(value: ByteParameter); external name '_LMSetSPVolCtl';
{
 *  LMGetSPClikCaret()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPClikCaret: ByteParameter; external name '_LMGetSPClikCaret';
{
 *  LMSetSPClikCaret()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPClikCaret(value: ByteParameter); external name '_LMSetSPClikCaret';
{
 *  LMGetSPMisc2()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSPMisc2: ByteParameter; external name '_LMGetSPMisc2';
{
 *  LMSetSPMisc2()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSPMisc2(value: ByteParameter); external name '_LMSetSPMisc2';
{  Carbon Usage: use GetDateTime }
{
 *  LMGetTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTime: SInt32; external name '_LMGetTime';
{  Carbon Usage: use SetDateTime }
{
 *  LMSetTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTime(value: SInt32); external name '_LMSetTime';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetBootDrive()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetBootDrive: SInt16; external name '_LMGetBootDrive';
{
 *  LMSetBootDrive()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetBootDrive(value: SInt16); external name '_LMSetBootDrive';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetSFSaveDisk()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSFSaveDisk: SInt16; external name '_LMGetSFSaveDisk';
{
 *  LMSetSFSaveDisk()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSFSaveDisk(value: SInt16); external name '_LMSetSFSaveDisk';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetMemErr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetMemErr: SInt16; external name '_LMGetMemErr';
{
 *  LMSetMemErr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetMemErr(value: SInt16); external name '_LMSetMemErr';
{
 *  LMGetSdVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetSdVolume: ByteParameter; external name '_LMGetSdVolume';
{
 *  LMSetSdVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetSdVolume(value: ByteParameter); external name '_LMSetSdVolume';
{
 *  LMGetSoundPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetSoundPtr: Ptr; external name '_LMGetSoundPtr';
{
 *  LMSetSoundPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetSoundPtr(value: Ptr); external name '_LMSetSoundPtr';
{
 *  LMGetSoundBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetSoundBase: Ptr; external name '_LMGetSoundBase';
{
 *  LMSetSoundBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetSoundBase(value: Ptr); external name '_LMSetSoundBase';
{
 *  LMGetSoundLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetSoundLevel: ByteParameter; external name '_LMGetSoundLevel';
{
 *  LMSetSoundLevel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetSoundLevel(value: ByteParameter); external name '_LMSetSoundLevel';
{
 *  LMGetCurPitch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetCurPitch: SInt16; external name '_LMGetCurPitch';
{
 *  LMSetCurPitch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetCurPitch(value: SInt16); external name '_LMSetCurPitch';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetROM85()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetROM85: SInt16; external name '_LMGetROM85';
{
 *  LMSetROM85()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetROM85(value: SInt16); external name '_LMSetROM85';
{
 *  LMGetPortBUse()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetPortBUse: ByteParameter; external name '_LMGetPortBUse';
{
 *  LMSetPortBUse()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetPortBUse(value: ByteParameter); external name '_LMSetPortBUse';
{
 *  LMGetGNEFilter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetGNEFilter: GetNextEventFilterUPP; external name '_LMGetGNEFilter';
{
 *  LMSetGNEFilter()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetGNEFilter(value: GetNextEventFilterUPP); external name '_LMSetGNEFilter';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetSysZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetSysZone: THz; external name '_LMGetSysZone';
{
 *  LMSetSysZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetSysZone(value: THz); external name '_LMSetSysZone';
{
 *  LMGetApplZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetApplZone: THz; external name '_LMGetApplZone';
{
 *  LMSetApplZone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetApplZone(value: THz); external name '_LMSetApplZone';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetROMBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetROMBase: Ptr; external name '_LMGetROMBase';
{
 *  LMSetROMBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetROMBase(value: Ptr); external name '_LMSetROMBase';
{
 *  LMGetRAMBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetRAMBase: Ptr; external name '_LMGetRAMBase';
{
 *  LMSetRAMBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetRAMBase(value: Ptr); external name '_LMSetRAMBase';
{
 *  LMGetDSAlertTab()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDSAlertTab: Ptr; external name '_LMGetDSAlertTab';
{
 *  LMSetDSAlertTab()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDSAlertTab(value: Ptr); external name '_LMSetDSAlertTab';
{
    NOTE:   LMGetABusVars and LMSetABusVars have been removed.
            Their implememtation in InterfaceLib was inconsistent
            with their prototypes here.  In InterfaceLib LMSetABusVars 
            would copy eight bytes and LMGetABusVars would return the
            value 0x02D8 instead of the long at that location.
            
            Use LMGetABusGlobals/LMSetABusGlobals to get/set the
            long at location 0x02D8 which is a pointer to the AppleTalk
            globals.  Use LMGetABusDCE/LMSetABusDCE to get/set the
            long at location 0x02DC which is the .MPP driver
            Device Control Entry. 
            
}
{
 *  LMGetABusGlobals()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetABusGlobals: Ptr; external name '_LMGetABusGlobals';
{
 *  LMGetABusDCE()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetABusDCE: Ptr; external name '_LMGetABusDCE';
{
 *  LMSetABusGlobals()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetABusGlobals(value: Ptr); external name '_LMSetABusGlobals';
{
 *  LMSetABusDCE()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetABusDCE(value: Ptr); external name '_LMSetABusDCE';
{  Carbon Usage: use GetDblTime }
{
 *  LMGetDoubleTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDoubleTime: UInt32; external name '_LMGetDoubleTime';
{
 *  LMSetDoubleTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDoubleTime(value: UInt32); external name '_LMSetDoubleTime';
{  Carbon Usage: use GetCaretTime }
{
 *  LMGetCaretTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetCaretTime: UInt32; external name '_LMGetCaretTime';
{
 *  LMSetCaretTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetCaretTime(value: UInt32); external name '_LMSetCaretTime';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetScrDmpEnb()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetScrDmpEnb: ByteParameter; external name '_LMGetScrDmpEnb';
{
 *  LMSetScrDmpEnb()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetScrDmpEnb(value: ByteParameter); external name '_LMSetScrDmpEnb';
{
 *  LMGetBufTgFNum()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetBufTgFNum: SInt32; external name '_LMGetBufTgFNum';
{
 *  LMSetBufTgFNum()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetBufTgFNum(value: SInt32); external name '_LMSetBufTgFNum';
{
 *  LMGetBufTgFFlg()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetBufTgFFlg: SInt16; external name '_LMGetBufTgFFlg';
{
 *  LMSetBufTgFFlg()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetBufTgFFlg(value: SInt16); external name '_LMSetBufTgFFlg';
{
 *  LMGetBufTgFBkNum()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetBufTgFBkNum: SInt16; external name '_LMGetBufTgFBkNum';
{
 *  LMSetBufTgFBkNum()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetBufTgFBkNum(value: SInt16); external name '_LMSetBufTgFBkNum';
{
 *  LMGetBufTgDate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetBufTgDate: SInt32; external name '_LMGetBufTgDate';
{
 *  LMSetBufTgDate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetBufTgDate(value: SInt32); external name '_LMSetBufTgDate';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetLo3Bytes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetLo3Bytes: SInt32; external name '_LMGetLo3Bytes';
{
 *  LMSetLo3Bytes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetLo3Bytes(value: SInt32); external name '_LMSetLo3Bytes';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetMinStack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetMinStack: SInt32; external name '_LMGetMinStack';
{
 *  LMSetMinStack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetMinStack(value: SInt32); external name '_LMSetMinStack';
{
 *  LMGetDefltStack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetDefltStack: SInt32; external name '_LMGetDefltStack';
{
 *  LMSetDefltStack()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetDefltStack(value: SInt32); external name '_LMSetDefltStack';
{
 *  LMGetGZRootHnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetGZRootHnd: Handle; external name '_LMGetGZRootHnd';
{
 *  LMSetGZRootHnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetGZRootHnd(value: Handle); external name '_LMSetGZRootHnd';
{
 *  LMGetGZMoveHnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetGZMoveHnd: Handle; external name '_LMGetGZMoveHnd';
{
 *  LMSetGZMoveHnd()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetGZMoveHnd(value: Handle); external name '_LMSetGZMoveHnd';
{
   LMGetFCBSPtr, LMSetFCBSPtr and LMSetFSFCBLen are not supported with Mac OS 9
   and later. Access to information in File Control Blocks or Fork Control Blocks
   (FCBs) should, if at all possible, be made with the GetFCBInfo or GetForkCBInfo
   routines. See the Technote "FCBs, Now and Forever" or the Technical Q&A
   "Accessing File Control Blocks" for complete information on this subject.
   Direct access to FCBs is not allowed by Carbon. Non-Carbon programs that
   require direct access to FCBs should use the File System Manager (FSM) FCB accessor
   functions if FSM is available (use the Gestalt selector gestaltFSAttr to determine
   this). Non-Carbon programs needing direct access to FCBs when FSM is not available
   can define ENABLE_FCB_ARRAY_ACCESS to be true when compiling.
}
{$ifc undefined ENABLE_FCB_ARRAY_ACCESS}
{$setc ENABLE_FCB_ARRAY_ACCESS := 0}
{$endc}

{$ifc ENABLE_FCB_ARRAY_ACCESS}
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetFCBSPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetFCBSPtr: Ptr; external name '_LMGetFCBSPtr';
{
 *  LMSetFCBSPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetFCBSPtr(value: Ptr); external name '_LMSetFCBSPtr';
{
 *  LMSetFSFCBLen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetFSFCBLen(value: SInt16); external name '_LMSetFSFCBLen';
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {ENABLE_FCB_ARRAY_ACCESS}

{
   LMGetFSFCBLen is supported only for the purpose of determining that the HFS
   file system is available as documented in developer Technotes (the HFS file system
   is available in System 3.2 and later). There is no documented use of FSFCBLen
   other than testing it for a positive value.
}
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetFSFCBLen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetFSFCBLen: SInt16; external name '_LMGetFSFCBLen';
{
 *  LMGetDefVCBPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDefVCBPtr: Ptr; external name '_LMGetDefVCBPtr';
{
 *  LMSetDefVCBPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDefVCBPtr(value: Ptr); external name '_LMSetDefVCBPtr';
{
 *  LMGetCurDirStore()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetCurDirStore: SInt32; external name '_LMGetCurDirStore';
{
 *  LMSetCurDirStore()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetCurDirStore(value: SInt32); external name '_LMSetCurDirStore';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetToExtFS()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetToExtFS: UniversalProcPtr; external name '_LMGetToExtFS';
{
 *  LMSetToExtFS()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetToExtFS(value: UniversalProcPtr); external name '_LMSetToExtFS';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetScrnBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetScrnBase: Ptr; external name '_LMGetScrnBase';
{
 *  LMSetScrnBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetScrnBase(value: Ptr); external name '_LMSetScrnBase';
{
 *  LMGetCrsrBusy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetCrsrBusy: ByteParameter; external name '_LMGetCrsrBusy';
{
 *  LMSetCrsrBusy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetCrsrBusy(value: ByteParameter); external name '_LMSetCrsrBusy';
{
 *  LMGetJournalRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetJournalRef: SInt16; external name '_LMGetJournalRef';
{
 *  LMSetJournalRef()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetJournalRef(value: SInt16); external name '_LMSetJournalRef';
{
 *  LMGetCrsrThresh()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetCrsrThresh: SInt16; external name '_LMGetCrsrThresh';
{
 *  LMSetCrsrThresh()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetCrsrThresh(value: SInt16); external name '_LMSetCrsrThresh';
{
 *  LMGetJFetch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetJFetch: UniversalProcPtr; external name '_LMGetJFetch';
{
 *  LMSetJFetch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetJFetch(value: UniversalProcPtr); external name '_LMSetJFetch';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetJStash()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetJStash: UniversalProcPtr; external name '_LMGetJStash';
{
 *  LMSetJStash()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetJStash(value: UniversalProcPtr); external name '_LMSetJStash';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetJIODone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetJIODone: UniversalProcPtr; external name '_LMGetJIODone';
{
 *  LMSetJIODone()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetJIODone(value: UniversalProcPtr); external name '_LMSetJIODone';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetCurApRefNum()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetCurApRefNum: SInt16; external name '_LMGetCurApRefNum';
{
 *  LMSetCurApRefNum()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetCurApRefNum(value: SInt16); external name '_LMSetCurApRefNum';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetCurrentA5()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetCurrentA5: Ptr; external name '_LMGetCurrentA5';
{
 *  LMSetCurrentA5()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetCurrentA5(value: Ptr); external name '_LMSetCurrentA5';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetCurStackBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetCurStackBase: Ptr; external name '_LMGetCurStackBase';
{
 *  LMSetCurStackBase()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetCurStackBase(value: Ptr); external name '_LMSetCurStackBase';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetCurJTOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetCurJTOffset: SInt16; external name '_LMGetCurJTOffset';
{
 *  LMSetCurJTOffset()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetCurJTOffset(value: SInt16); external name '_LMSetCurJTOffset';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetCurPageOption()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetCurPageOption: SInt16; external name '_LMGetCurPageOption';
{
 *  LMSetCurPageOption()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetCurPageOption(value: SInt16); external name '_LMSetCurPageOption';
{
 *  LMGetPrintErr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetPrintErr: SInt16; external name '_LMGetPrintErr';
{
 *  LMSetPrintErr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetPrintErr(value: SInt16); external name '_LMSetPrintErr';
{   Carbon Scrap Manager does not support low memory. }

{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetScrapSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetScrapSize: SInt32; external name '_LMGetScrapSize';
{
 *  LMSetScrapSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetScrapSize(value: SInt32); external name '_LMSetScrapSize';
{
 *  LMGetScrapHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetScrapHandle: Handle; external name '_LMGetScrapHandle';
{
 *  LMSetScrapHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetScrapHandle(value: Handle); external name '_LMSetScrapHandle';
{
 *  LMGetScrapCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetScrapCount: SInt16; external name '_LMGetScrapCount';
{
 *  LMSetScrapCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetScrapCount(value: SInt16); external name '_LMSetScrapCount';
{
 *  LMGetScrapState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetScrapState: SInt16; external name '_LMGetScrapState';
{
 *  LMSetScrapState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetScrapState(value: SInt16); external name '_LMSetScrapState';
{
 *  LMGetScrapName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetScrapName: StringPtr; external name '_LMGetScrapName';
{
 *  LMSetScrapName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetScrapName(value: StringPtr); external name '_LMSetScrapName';
{
 *  LMGetROMFont0()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetROMFont0: Handle; external name '_LMGetROMFont0';
{
 *  LMSetROMFont0()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetROMFont0(value: Handle); external name '_LMSetROMFont0';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetApFontID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetApFontID: SInt16; external name '_LMGetApFontID';
{
 *  LMSetApFontID()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetApFontID(value: SInt16); external name '_LMSetApFontID';
{ Carbon versions of the Window Manager do not support LowMem. }
{  Carbon Usage: use GetWindowList }
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetWindowList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetWindowList: WindowRef; external name '_LMGetWindowList';
{
 *  LMGetSaveUpdate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSaveUpdate: SInt16; external name '_LMGetSaveUpdate';
{
 *  LMSetSaveUpdate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSaveUpdate(value: SInt16); external name '_LMSetSaveUpdate';
{
 *  LMGetPaintWhite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetPaintWhite: SInt16; external name '_LMGetPaintWhite';
{  Carbon Usage : use InstallWindowContentPaintProc }
{
 *  LMSetPaintWhite()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetPaintWhite(value: SInt16); external name '_LMSetPaintWhite';
{
 *  LMGetWMgrPort()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetWMgrPort: GrafPtr; external name '_LMGetWMgrPort';
{
 *  LMSetWMgrPort()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetWMgrPort(value: GrafPtr); external name '_LMSetWMgrPort';
{  Carbon Usage: use GetGrayRgn }
{
 *  LMGetGrayRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetGrayRgn: RgnHandle; external name '_LMGetGrayRgn';
{
 *  LMGetDragHook()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDragHook: DragGrayRgnUPP; external name '_LMGetDragHook';
{
 *  LMSetDragHook()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDragHook(value: DragGrayRgnUPP); external name '_LMSetDragHook';
{
 *  LMSetWindowList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetWindowList(value: WindowRef); external name '_LMSetWindowList';
{
 *  LMGetGhostWindow()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetGhostWindow: WindowRef; external name '_LMGetGhostWindow';
{
 *  LMSetGhostWindow()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetGhostWindow(value: WindowRef); external name '_LMSetGhostWindow';
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc NOT OPAQUE_TOOLBOX_STRUCTS}
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetAuxWinHead()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetAuxWinHead: AuxWinHandle; external name '_LMGetAuxWinHead';
{
 *  LMSetAuxWinHead()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetAuxWinHead(value: AuxWinHandle); external name '_LMSetAuxWinHead';
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}

{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetCurActivate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetCurActivate: WindowRef; external name '_LMGetCurActivate';
{
 *  LMSetCurActivate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetCurActivate(value: WindowRef); external name '_LMSetCurActivate';
{
 *  LMGetCurDeactive()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetCurDeactive: WindowRef; external name '_LMGetCurDeactive';
{
 *  LMSetCurDeactive()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetCurDeactive(value: WindowRef); external name '_LMSetCurDeactive';
{
 *  LMGetOldStructure()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetOldStructure: RgnHandle; external name '_LMGetOldStructure';
{
 *  LMSetOldStructure()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetOldStructure(value: RgnHandle); external name '_LMSetOldStructure';
{
 *  LMGetOldContent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetOldContent: RgnHandle; external name '_LMGetOldContent';
{
 *  LMSetOldContent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetOldContent(value: RgnHandle); external name '_LMSetOldContent';
{
 *  LMSetGrayRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetGrayRgn(value: RgnHandle); external name '_LMSetGrayRgn';
{
 *  LMGetSaveVisRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSaveVisRgn: RgnHandle; external name '_LMGetSaveVisRgn';
{
 *  LMSetSaveVisRgn()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSaveVisRgn(value: RgnHandle); external name '_LMSetSaveVisRgn';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetOneOne()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetOneOne: SInt32; external name '_LMGetOneOne';
{
 *  LMSetOneOne()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetOneOne(value: SInt32); external name '_LMSetOneOne';
{
 *  LMGetMinusOne()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetMinusOne: SInt32; external name '_LMGetMinusOne';
{
 *  LMSetMinusOne()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetMinusOne(value: SInt32); external name '_LMSetMinusOne';
{  Carbon Usage: use GetMenuTrackingData }
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetTopMenuItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTopMenuItem: SInt16; external name '_LMGetTopMenuItem';
{  Carbon Usage: replaced by MDEF messages and GetMenuTrackingData API }
{
 *  LMSetTopMenuItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTopMenuItem(value: SInt16); external name '_LMSetTopMenuItem';
{  Carbon Usage: use GetMenuTrackingData }
{
 *  LMGetAtMenuBottom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetAtMenuBottom: SInt16; external name '_LMGetAtMenuBottom';
{  Carbon Usage: replaced by MDEF messages and GetMenuTrackingData API }
{
 *  LMSetAtMenuBottom()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetAtMenuBottom(value: SInt16); external name '_LMSetAtMenuBottom';
{
   Carbon usage: use GetMenuBar (which returns a newly allocated handle in
   the same format as that returned by LMGetMenuList; dispose with DisposeMenuBar)
   or GetRootMenu.
}
{
 *  LMGetMenuList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMenuList: Handle; external name '_LMGetMenuList';
{
 *  LMSetMenuList()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMenuList(value: Handle); external name '_LMSetMenuList';
{  Carbon usage: no replacement }
{
 *  LMGetMBarEnable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMBarEnable: SInt16; external name '_LMGetMBarEnable';
{
 *  LMSetMBarEnable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMBarEnable(value: SInt16); external name '_LMSetMBarEnable';
{  Carbon usage: no replacement }
{
 *  LMGetMenuFlash()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMenuFlash: SInt16; external name '_LMGetMenuFlash';
{
 *  LMSetMenuFlash()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMenuFlash(value: SInt16); external name '_LMSetMenuFlash';
{ LMGetTheMenu() moved to Menus.h }
{
 *  LMSetTheMenu()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTheMenu(value: SInt16); external name '_LMSetTheMenu';
{
 *  LMGetMBarHook()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMBarHook: MBarHookUPP; external name '_LMGetMBarHook';
{
 *  LMSetMBarHook()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMBarHook(value: MBarHookUPP); external name '_LMSetMBarHook';
{
 *  LMGetMenuHook()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMenuHook: MenuHookUPP; external name '_LMGetMenuHook';
{
 *  LMSetMenuHook()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMenuHook(value: MenuHookUPP); external name '_LMSetMenuHook';
{
 *  LMGetTopMapHndl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTopMapHndl: Handle; external name '_LMGetTopMapHndl';
{
 *  LMSetTopMapHndl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTopMapHndl(value: Handle); external name '_LMSetTopMapHndl';
{
 *  LMGetSysMapHndl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSysMapHndl: Handle; external name '_LMGetSysMapHndl';
{
 *  LMSetSysMapHndl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSysMapHndl(value: Handle); external name '_LMSetSysMapHndl';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetSysMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetSysMap: SInt16; external name '_LMGetSysMap';
{
 *  LMSetSysMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetSysMap(value: SInt16); external name '_LMSetSysMap';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetCurMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetCurMap: SInt16; external name '_LMGetCurMap';
{
 *  LMSetCurMap()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetCurMap(value: SInt16); external name '_LMSetCurMap';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetResLoad()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetResLoad: ByteParameter; external name '_LMGetResLoad';
{
 *  LMSetResLoad()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetResLoad(value: ByteParameter); external name '_LMSetResLoad';
{
 *  LMGetResErr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetResErr: SInt16; external name '_LMGetResErr';
{
 *  LMSetResErr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetResErr(value: SInt16); external name '_LMSetResErr';
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetFScaleDisable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetFScaleDisable: ByteParameter; external name '_LMGetFScaleDisable';
{
 *  LMSetFScaleDisable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetFScaleDisable(value: ByteParameter); external name '_LMSetFScaleDisable';
{
 *  LMGetDeskHook()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDeskHook: UniversalProcPtr; external name '_LMGetDeskHook';
{
 *  LMSetDeskHook()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDeskHook(value: UniversalProcPtr); external name '_LMSetDeskHook';
{  Carbon Usage: Use TEGetDoTextHook. }
{
 *  LMGetTEDoText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTEDoText: UniversalProcPtr; external name '_LMGetTEDoText';
{  Carbon Usage: Use TESetDoTextHook. }
{
 *  LMSetTEDoText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTEDoText(value: UniversalProcPtr); external name '_LMSetTEDoText';
{  Carbon Usage: Use TEGetRecalcHook. }
{
 *  LMGetTERecal()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTERecal: UniversalProcPtr; external name '_LMGetTERecal';
{  Carbon Usage: Use TESetRecalcHook. }
{
 *  LMSetTERecal()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTERecal(value: UniversalProcPtr); external name '_LMSetTERecal';
{
 *  LMGetResumeProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetResumeProc: UniversalProcPtr; external name '_LMGetResumeProc';
{
 *  LMSetResumeProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetResumeProc(value: UniversalProcPtr); external name '_LMSetResumeProc';
{
 *  LMGetANumber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetANumber: SInt16; external name '_LMGetANumber';
{
 *  LMSetANumber()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetANumber(value: SInt16); external name '_LMSetANumber';
{  Carbon Usage: Use GetAlertStage. }
{
 *  LMGetACount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetACount: SInt16; external name '_LMGetACount';
{  Carbon Usage: Use ResetAlertStage. }
{
 *  LMSetACount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetACount(value: SInt16); external name '_LMSetACount';
{
 *  LMGetDABeeper()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDABeeper: UniversalProcPtr; external name '_LMGetDABeeper';
{
 *  LMSetDABeeper()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDABeeper(value: UniversalProcPtr); external name '_LMSetDABeeper';
{  Carbon Usage: use TEGetScrapLength }
{
 *  LMGetTEScrpLength()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTEScrpLength: UInt16; external name '_LMGetTEScrpLength';
{  Carbon Usage: use TESetScrapLength }
{
 *  LMSetTEScrpLength()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTEScrpLength(value: UInt16); external name '_LMSetTEScrpLength';
{  Carbon Usage: use TEGetScrapHandle }
{
 *  LMGetTEScrpHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTEScrpHandle: Handle; external name '_LMGetTEScrpHandle';
{  Carbon Usage: use TESetScrapHandle }
{
 *  LMSetTEScrpHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTEScrpHandle(value: Handle); external name '_LMSetTEScrpHandle';
{
 *  LMGetAppParmHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetAppParmHandle: Handle; external name '_LMGetAppParmHandle';
{
 *  LMSetAppParmHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetAppParmHandle(value: Handle); external name '_LMSetAppParmHandle';
{
 *  LMGetDSErrCode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDSErrCode: SInt16; external name '_LMGetDSErrCode';
{
 *  LMSetDSErrCode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDSErrCode(value: SInt16); external name '_LMSetDSErrCode';
{
 *  LMGetResErrProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetResErrProc: ResErrUPP; external name '_LMGetResErrProc';
{
 *  LMSetResErrProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetResErrProc(value: ResErrUPP); external name '_LMSetResErrProc';
{
 *  LMGetDlgFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDlgFont: SInt16; external name '_LMGetDlgFont';
{  Carbon Usage: use SetDialogFont }
{
 *  LMSetDlgFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDlgFont(value: SInt16); external name '_LMSetDlgFont';
{
 *  LMGetATalkHk2()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetATalkHk2: Ptr; external name '_LMGetATalkHk2';
{
 *  LMSetATalkHk2()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetATalkHk2(value: Ptr); external name '_LMSetATalkHk2';
{
 *  LMGetHWCfgFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetHWCfgFlags: SInt16; external name '_LMGetHWCfgFlags';
{
 *  LMSetHWCfgFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetHWCfgFlags(value: SInt16); external name '_LMSetHWCfgFlags';
{  Carbon Usage: use GetMenuTrackingData }
{
 *  LMGetMenuDisable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMenuDisable: SInt32; external name '_LMGetMenuDisable';
{  Carbon Usage: use new MDEF messages }
{
 *  LMSetMenuDisable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMenuDisable(value: SInt32); external name '_LMSetMenuDisable';
{
 *  LMGetROMMapInsert()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetROMMapInsert: ByteParameter; external name '_LMGetROMMapInsert';
{
 *  LMSetROMMapInsert()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetROMMapInsert(value: ByteParameter); external name '_LMSetROMMapInsert';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetTmpResLoad()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetTmpResLoad: ByteParameter; external name '_LMGetTmpResLoad';
{
 *  LMSetTmpResLoad()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetTmpResLoad(value: ByteParameter); external name '_LMSetTmpResLoad';
{
 *  LMGetIntlSpec()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetIntlSpec: Ptr; external name '_LMGetIntlSpec';
{
 *  LMSetIntlSpec()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetIntlSpec(value: Ptr); external name '_LMSetIntlSpec';
{ LMGetWordRedraw and LMSetWordRedraw moved to TextEdit.h }
{
 *  LMGetSysFontFam()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetSysFontFam: SInt16; external name '_LMGetSysFontFam';
{
 *  LMSetSysFontFam()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetSysFontFam(value: SInt16); external name '_LMSetSysFontFam';
{
 *  LMGetSysFontSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetSysFontSize: SInt16; external name '_LMGetSysFontSize';
{
 *  LMSetSysFontSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetSysFontSize(value: SInt16); external name '_LMSetSysFontSize';
{  Carbon Usge: use GetMBarHeight }
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetMBarHeight()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMBarHeight: SInt16; external name '_LMGetMBarHeight';
{  Carbon Usage: use Hide/ShowMenuBar }
{
 *  LMSetMBarHeight()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMBarHeight(value: SInt16); external name '_LMSetMBarHeight';
{
 *  LMGetTESysJust()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTESysJust: SInt16; external name '_LMGetTESysJust';
{
 *  LMSetTESysJust()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTESysJust(value: SInt16); external name '_LMSetTESysJust';
{
 *  LMGetMMU32Bit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMMU32Bit: ByteParameter; external name '_LMGetMMU32Bit';
{
 *  LMSetMMU32Bit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMMU32Bit(value: ByteParameter); external name '_LMSetMMU32Bit';
{
 *  LMGetDeskCPat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDeskCPat: PixPatHandle; external name '_LMGetDeskCPat';
{
 *  LMSetDeskCPat()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDeskCPat(value: PixPatHandle); external name '_LMSetDeskCPat';
{
 *  LMGetTimeDBRA()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTimeDBRA: SInt16; external name '_LMGetTimeDBRA';
{
 *  LMSetTimeDBRA()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTimeDBRA(value: SInt16); external name '_LMSetTimeDBRA';
{
 *  LMGetTimeSCCDB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTimeSCCDB: SInt16; external name '_LMGetTimeSCCDB';
{
 *  LMSetTimeSCCDB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTimeSCCDB(value: SInt16); external name '_LMSetTimeSCCDB';
{
 *  LMGetJVBLTask()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetJVBLTask: UniversalProcPtr; external name '_LMGetJVBLTask';
{
 *  LMSetJVBLTask()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetJVBLTask(value: UniversalProcPtr); external name '_LMSetJVBLTask';
{
 *  LMGetSynListHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSynListHandle: Handle; external name '_LMGetSynListHandle';
{
 *  LMSetSynListHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetSynListHandle(value: Handle); external name '_LMSetSynListHandle';
{
 *  LMGetMenuCInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMenuCInfo: MCTableHandle; external name '_LMGetMenuCInfo';
{
 *  LMSetMenuCInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMenuCInfo(value: MCTableHandle); external name '_LMSetMenuCInfo';
{
 *  LMGetJDTInstall()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetJDTInstall: UniversalProcPtr; external name '_LMGetJDTInstall';
{
 *  LMSetJDTInstall()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetJDTInstall(value: UniversalProcPtr); external name '_LMSetJDTInstall';
{
 *  LMGetTimeSCSIDB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetTimeSCSIDB: SInt16; external name '_LMGetTimeSCSIDB';
{
 *  LMSetTimeSCSIDB()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTimeSCSIDB(value: SInt16); external name '_LMSetTimeSCSIDB';
{*************************************************************************************

    MORE COMPLEX LOWMEM ACCESSORS

*************************************************************************************}
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc TARGET_CPU_68K AND NOT TARGET_RT_MAC_CFM}
{*************************************************************************************
    "BIG DATA"
    
        These lowmem accessors access big (> 4 bytes) values.
*************************************************************************************}
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetDeskPattern()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMGetDeskPattern(var deskPatternValue: Pattern); external name '_LMGetDeskPattern';
{
 *  LMSetDeskPattern()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDeskPattern(const (*var*) deskPatternValue: Pattern); external name '_LMSetDeskPattern';
{$endc}  {CALL_NOT_IN_CARBON}
{*************************************************************************************
    "BLOCKMOVE ACCESSORS"
    
        These lowmem accessors use the BlockMove to set
*************************************************************************************}
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetCurApName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetCurApName: StringPtr; external name '_LMGetCurApName';
{
 *  LMGetSysResName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetSysResName: StringPtr; external name '_LMGetSysResName';
{
 *  LMGetFinderName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetFinderName: StringPtr; external name '_LMGetFinderName';
{$endc}  {CALL_NOT_IN_CARBON}
{*************************************************************************************
    "INDEXED ACCESSORS"
    
        These lowmem accessors take an index parameter to get/set an indexed
        lowmem global.
*************************************************************************************}
{************************************************************************************
    The DAString accessors are gone with Carbon. Please use ParamText and
    GetParamText instead.
*************************************************************************************}
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetDAStrings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDAStrings(whichString: SInt16): StringHandle; external name '_LMGetDAStrings';
{
 *  LMSetDAStrings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDAStrings(stringsValue: StringHandle; whichString: SInt16); external name '_LMSetDAStrings';
{$endc}  {CALL_NOT_IN_CARBON}

{$elsec}
{*************************************************************************************
    "BIG DATA"
    
        These lowmem accessors access big (> 4 bytes) values.
*************************************************************************************}

{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetDSAlertRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMGetDSAlertRect(var dsAlertRectValue: Rect); external name '_LMGetDSAlertRect';

{
 *  LMSetDSAlertRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDSAlertRect(const (*var*) dsAlertRectValue: Rect); external name '_LMSetDSAlertRect';

{
 *  LMGetDragPattern()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMGetDragPattern(var dragPatternValue: Pattern); external name '_LMGetDragPattern';

{
 *  LMSetDragPattern()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDragPattern(const (*var*) dragPatternValue: Pattern); external name '_LMSetDragPattern';

{
 *  LMGetDeskPattern()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMGetDeskPattern(var deskPatternValue: Pattern); external name '_LMGetDeskPattern';

{
 *  LMSetDeskPattern()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDeskPattern(const (*var*) deskPatternValue: Pattern); external name '_LMSetDeskPattern';

{
 *  LMGetEventQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetEventQueue: QHdrPtr; external name '_LMGetEventQueue';

{
 *  LMSetEventQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetEventQueue(eventQueueValue: QHdrPtr); external name '_LMSetEventQueue';


{
 *  LMGetVBLQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetVBLQueue: QHdrPtr; external name '_LMGetVBLQueue';

{
 *  LMSetVBLQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetVBLQueue(vblQueueValue: QHdrPtr); external name '_LMSetVBLQueue';

{
 *  LMGetDrvQHdr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDrvQHdr: QHdrPtr; external name '_LMGetDrvQHdr';

{
 *  LMSetDrvQHdr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDrvQHdr(drvQHdrValue: QHdrPtr); external name '_LMSetDrvQHdr';

{
 *  LMGetVCBQHdr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetVCBQHdr: QHdrPtr; external name '_LMGetVCBQHdr';

{
 *  LMSetVCBQHdr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetVCBQHdr(vcbQHdrValue: QHdrPtr); external name '_LMSetVCBQHdr';

{
 *  LMGetDTQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDTQueue: QHdrPtr; external name '_LMGetDTQueue';

{
 *  LMSetDTQueue()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDTQueue(dtQueueValue: QHdrPtr); external name '_LMSetDTQueue';

{
 *  LMGetFSQHdr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetFSQHdr: QHdrPtr; external name '_LMGetFSQHdr';

{*************************************************************************************
    "BLOCKMOVE ACCESSORS"
    
        These lowmem accessors use the BlockMove to set
*************************************************************************************}
{$endc}  {CALL_NOT_IN_CARBON}
{
 *  LMGetCurApName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetCurApName: StringPtr; external name '_LMGetCurApName';

{
 *  LMSetCurApName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetCurApName(const (*var*) curApNameValue: Str31); external name '_LMSetCurApName';

{
 *  LMGetSysResName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetSysResName: StringPtr; external name '_LMGetSysResName';

{
 *  LMSetSysResName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetSysResName(const (*var*) sysResNameValue: Str15); external name '_LMSetSysResName';

{
 *  LMGetFinderName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetFinderName: StringPtr; external name '_LMGetFinderName';

{
 *  LMSetFinderName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetFinderName(const (*var*) finderNameValue: Str15); external name '_LMSetFinderName';

{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetScratch20()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetScratch20: Ptr; external name '_LMGetScratch20';

{
 *  LMSetScratch20()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetScratch20(scratch20Value: UnivPtr); external name '_LMSetScratch20';

{$endc}  {CALL_NOT_IN_CARBON}
{
 *  LMGetToolScratch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetToolScratch: Ptr; external name '_LMGetToolScratch';

{
 *  LMSetToolScratch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetToolScratch(toolScratchValue: UnivPtr); external name '_LMSetToolScratch';

{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetApplScratch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetApplScratch: Ptr; external name '_LMGetApplScratch';

{
 *  LMSetApplScratch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetApplScratch(applScratchValue: UnivPtr); external name '_LMSetApplScratch';


{*************************************************************************************
    "INDEXED ACCESSORS"
    
        These lowmem accessors take an index parameter to get/set an indexed
        lowmem global.
*************************************************************************************}

{  Carbon Usage: use GetParamText }
{
 *  LMGetDAStrings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetDAStrings(whichString: SInt16): StringHandle; external name '_LMGetDAStrings';

{  Carbon Usage: use ParamText }
{
 *  LMSetDAStrings()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetDAStrings(stringsValue: StringHandle; whichString: SInt16); external name '_LMSetDAStrings';


{$endc}  {CALL_NOT_IN_CARBON}
{
 *  LMGetLvl2DT()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetLvl2DT(vectorNumber: SInt16): UniversalProcPtr; external name '_LMGetLvl2DT';

{
 *  LMSetLvl2DT()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetLvl2DT(Lvl2DTValue: UniversalProcPtr; vectorNumber: SInt16); external name '_LMSetLvl2DT';

{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetExtStsDT()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetExtStsDT(vectorNumber: SInt16): UniversalProcPtr; external name '_LMGetExtStsDT';

{
 *  LMSetExtStsDT()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetExtStsDT(ExtStsDTValue: UniversalProcPtr; vectorNumber: SInt16); external name '_LMSetExtStsDT';

{$endc}  {CALL_NOT_IN_CARBON}
{$endc}

{*************************************************************************************
    "Missing Accessors"
    
        These lowmem accessors are not in the original InterfaceLib.  They were
        added to InterfaceLib in Mac OS 8.5.  In Universal Interfaces 3.2 they
        were defined via a macro. In you want to use these functions on a pre-8.5
        systems, you must write your own macros to override the function prototype
        or write your own implementation.
    
*************************************************************************************}


{$ifc CALL_NOT_IN_CARBON}
{
 *  LMSetMouseTemp()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMouseTemp(value: Point); external name '_LMSetMouseTemp';
{  accesses "MTemp" }
{
 *  LMGetMouseTemp()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMouseTemp: Point; external name '_LMGetMouseTemp';
{
   accesses "MBState"
   Carbon Usage: use Button()
}
{
 *  LMGetMouseButtonState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMouseButtonState: ByteParameter; external name '_LMGetMouseButtonState';
{
 *  LMSetMouseButtonState()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMouseButtonState(value: ByteParameter); external name '_LMSetMouseButtonState';
{
   accesses "RawMouse"
   Carbon Usage: use GetGlobalMouse
}
{
 *  LMGetRawMouseLocation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetRawMouseLocation: Point; external name '_LMGetRawMouseLocation';
{  Carbon Usage: use GetGlobalMouse }
{
 *  LMSetRawMouseLocation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetRawMouseLocation(value: Point); external name '_LMSetRawMouseLocation';
{
   accesses "Mouse"
   Carbon Usage: use GetGlobalMouse
}
{
 *  LMGetMouseLocation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMouseLocation: Point; external name '_LMGetMouseLocation';
{
 *  LMSetMouseLocation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMouseLocation(value: Point); external name '_LMSetMouseLocation';
{  accesses "TheCrsr" }
{
 *  LMGetTheCursor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMGetTheCursor(var currentCursor: Cursor); external name '_LMGetTheCursor';

{
 *  LMSetTheCursor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetTheCursor(const (*var*) newCursor: Cursor); external name '_LMSetTheCursor';


{  accesses "HiHeapMark" }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetHighHeapMark()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetHighHeapMark: Ptr; external name '_LMGetHighHeapMark';
{
 *  LMSetHighHeapMark()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetHighHeapMark(value: Ptr); external name '_LMSetHighHeapMark';
{  accesses "StkLowPt" }
{
 *  LMGetStackLowPoint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetStackLowPoint: Ptr; external name '_LMGetStackLowPoint';
{
 *  LMSetStackLowPoint()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetStackLowPoint(value: Ptr); external name '_LMSetStackLowPoint';
{  accesses "ROMMapHndl" }
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetROMMapHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetROMMapHandle: Handle; external name '_LMGetROMMapHandle';
{
 *  LMSetROMMapHandle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetROMMapHandle(value: Handle); external name '_LMSetROMMapHandle';
{  accesses "UnitNtryCnt" }
{
 *  LMGetUnitTableEntryCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetUnitTableEntryCount: SInt16; external name '_LMGetUnitTableEntryCount';
{
 *  LMSetUnitTableEntryCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetUnitTableEntryCount(value: SInt16); external name '_LMSetUnitTableEntryCount';
{  accesses "FmtDefaults" }
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  LMGetDiskFormatingHFSDefaults()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function LMGetDiskFormatingHFSDefaults: Ptr; external name '_LMGetDiskFormatingHFSDefaults';
{
 *  LMSetDiskFormatingHFSDefaults()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure LMSetDiskFormatingHFSDefaults(value: Ptr); external name '_LMSetDiskFormatingHFSDefaults';
{  accesses "PortAUse" }
{$ifc CALL_NOT_IN_CARBON}
{
 *  LMGetPortAInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetPortAInfo: ByteParameter; external name '_LMGetPortAInfo';
{
 *  LMSetPortAInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetPortAInfo(value: ByteParameter); external name '_LMSetPortAInfo';
{
 *  LMGetMBTicks()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetMBTicks: SInt32; external name '_LMGetMBTicks';
{
 *  LMSetMBTicks()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetMBTicks(value: SInt32); external name '_LMSetMBTicks';
{
 *  LMGetKeyTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function LMGetKeyTime: SInt32; external name '_LMGetKeyTime';
{
 *  LMSetKeyTime()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure LMSetKeyTime(value: SInt32); external name '_LMSetKeyTime';
{$endc}  {CALL_NOT_IN_CARBON}

{$ALIGN MAC68K}


end.
