{
     File:       CarbonCore/Collections.h
 
     Contains:   Collection Manager Interfaces
                 The contents of this header file are deprecated.
                 Use Foundation or CoreFoundation collection objects instead.
 
     Copyright:  © 1989-2011 by Apple Inc. All rights reserved.
}
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$modeswitch cblocks}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit Collections;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0400}
{$setc GAP_INTERFACES_VERSION := $0308}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC32}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __ppc64__ and defined CPUPOWERPC64}
	{$setc __ppc64__ := 1}
{$elsec}
	{$setc __ppc64__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}
{$ifc not defined __x86_64__ and defined CPUX86_64}
	{$setc __x86_64__ := 1}
{$elsec}
	{$setc __x86_64__ := 0}
{$endc}
{$ifc not defined __arm__ and defined CPUARM}
	{$setc __arm__ := 1}
{$elsec}
	{$setc __arm__ := 0}
{$endc}
{$ifc not defined __arm64__ and defined CPUAARCH64}
  {$setc __arm64__ := 1}
{$elsec}
  {$setc __arm64__ := 0}
{$endc}

{$ifc defined cpu64}
  {$setc __LP64__ := 1}
{$elsec}
  {$setc __LP64__ := 0}
{$endc}


{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elifc defined __arm64__ and __arm64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := TRUE}
{$ifc defined ios}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$endc}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ nor __arm64__ is defined.}
{$endc}

{$ifc defined __LP64__ and __LP64__ }
  {$setc TARGET_CPU_64 := TRUE}
{$elsec}
  {$setc TARGET_CPU_64 := FALSE}
{$endc}

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
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes;
{$endc} {not MACOSALLINCLUDE}


{$ifc TARGET_OS_MAC}

{$ALIGN POWER}


{***********}
{ Constants }
{***********}
{ Convenience constants for functions which optionally return values }
const
	kCollectionDontWantTag = 0;
	kCollectionDontWantId = 0;
	kCollectionDontWantSize = 0;
	kCollectionDontWantAttributes = 0;
	kCollectionDontWantIndex = 0;
	kCollectionDontWantData = 0;


{ attributes bits }
const
	kCollectionNoAttributes = $00000000; { no attributes bits set }
	kCollectionAllAttributes = -1; { all attributes bits set }
	kCollectionUserAttributes = $0000FFFF; { user attributes bits }
	kCollectionDefaultAttributes = $40000000; { default attributes - unlocked, persistent }


{ 
    Attribute bits 0 through 15 (entire low word) are reserved for use by the application.
    Attribute bits 16 through 31 (entire high word) are reserved for use by the Collection Manager.
    Only bits 31 (kCollectionLockBit) and 30 (kCollectionPersistenceBit) currently have meaning.
}
const
	kCollectionUser0Bit = 0;
	kCollectionUser1Bit = 1;
	kCollectionUser2Bit = 2;
	kCollectionUser3Bit = 3;
	kCollectionUser4Bit = 4;
	kCollectionUser5Bit = 5;
	kCollectionUser6Bit = 6;
	kCollectionUser7Bit = 7;
	kCollectionUser8Bit = 8;
	kCollectionUser9Bit = 9;
	kCollectionUser10Bit = 10;
	kCollectionUser11Bit = 11;
	kCollectionUser12Bit = 12;
	kCollectionUser13Bit = 13;
	kCollectionUser14Bit = 14;
	kCollectionUser15Bit = 15;
	kCollectionReserved0Bit = 16;
	kCollectionReserved1Bit = 17;
	kCollectionReserved2Bit = 18;
	kCollectionReserved3Bit = 19;
	kCollectionReserved4Bit = 20;
	kCollectionReserved5Bit = 21;
	kCollectionReserved6Bit = 22;
	kCollectionReserved7Bit = 23;
	kCollectionReserved8Bit = 24;
	kCollectionReserved9Bit = 25;
	kCollectionReserved10Bit = 26;
	kCollectionReserved11Bit = 27;
	kCollectionReserved12Bit = 28;
	kCollectionReserved13Bit = 29;
	kCollectionPersistenceBit = 30;
	kCollectionLockBit = 31;


{ attribute masks }
const
	kCollectionUser0Mask = 1 shl kCollectionUser0Bit;
	kCollectionUser1Mask = 1 shl kCollectionUser1Bit;
	kCollectionUser2Mask = 1 shl kCollectionUser2Bit;
	kCollectionUser3Mask = 1 shl kCollectionUser3Bit;
	kCollectionUser4Mask = 1 shl kCollectionUser4Bit;
	kCollectionUser5Mask = 1 shl kCollectionUser5Bit;
	kCollectionUser6Mask = 1 shl kCollectionUser6Bit;
	kCollectionUser7Mask = 1 shl kCollectionUser7Bit;
	kCollectionUser8Mask = 1 shl kCollectionUser8Bit;
	kCollectionUser9Mask = 1 shl kCollectionUser9Bit;
	kCollectionUser10Mask = 1 shl kCollectionUser10Bit;
	kCollectionUser11Mask = 1 shl kCollectionUser11Bit;
	kCollectionUser12Mask = 1 shl kCollectionUser12Bit;
	kCollectionUser13Mask = 1 shl kCollectionUser13Bit;
	kCollectionUser14Mask = 1 shl kCollectionUser14Bit;
	kCollectionUser15Mask = 1 shl kCollectionUser15Bit;
	kCollectionReserved0Mask = 1 shl kCollectionReserved0Bit;
	kCollectionReserved1Mask = 1 shl kCollectionReserved1Bit;
	kCollectionReserved2Mask = 1 shl kCollectionReserved2Bit;
	kCollectionReserved3Mask = 1 shl kCollectionReserved3Bit;
	kCollectionReserved4Mask = 1 shl kCollectionReserved4Bit;
	kCollectionReserved5Mask = 1 shl kCollectionReserved5Bit;
	kCollectionReserved6Mask = 1 shl kCollectionReserved6Bit;
	kCollectionReserved7Mask = 1 shl kCollectionReserved7Bit;
	kCollectionReserved8Mask = 1 shl kCollectionReserved8Bit;
	kCollectionReserved9Mask = 1 shl kCollectionReserved9Bit;
	kCollectionReserved10Mask = 1 shl kCollectionReserved10Bit;
	kCollectionReserved11Mask = 1 shl kCollectionReserved11Bit;
	kCollectionReserved12Mask = 1 shl kCollectionReserved12Bit;
	kCollectionReserved13Mask = 1 shl kCollectionReserved13Bit;
	kCollectionPersistenceMask = 1 shl kCollectionPersistenceBit;
	kCollectionLockMask = 1 shl kCollectionLockBit;


{*********}
{ Types   }
{*********}
{ abstract data type for a collection }
type
	Collection = ^OpaqueCollection; { an opaque type }
	OpaqueCollection = record end;
{ collection member 4 byte tag }
type
	CollectionTag = FourCharCode;
	CollectionFlattenProcPtr = function( size: SInt32; data: UnivPtr; refCon: UnivPtr ): OSErr;
	CollectionExceptionProcPtr = function( c: Collection; status: OSErr ): OSErr;
	CollectionFlattenUPP = CollectionFlattenProcPtr;
	CollectionExceptionUPP = CollectionExceptionProcPtr;
{
 *  NewCollectionFlattenUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewCollectionFlattenUPP( userRoutine: CollectionFlattenProcPtr ): CollectionFlattenUPP; external name '_NewCollectionFlattenUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  NewCollectionExceptionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function NewCollectionExceptionUPP( userRoutine: CollectionExceptionProcPtr ): CollectionExceptionUPP; external name '_NewCollectionExceptionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeCollectionFlattenUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeCollectionFlattenUPP( userUPP: CollectionFlattenUPP ); external name '_DisposeCollectionFlattenUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  DisposeCollectionExceptionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
procedure DisposeCollectionExceptionUPP( userUPP: CollectionExceptionUPP ); external name '_DisposeCollectionExceptionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeCollectionFlattenUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeCollectionFlattenUPP( size: SInt32; data: UnivPtr; refCon: UnivPtr; userUPP: CollectionFlattenUPP ): OSErr; external name '_InvokeCollectionFlattenUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{
 *  InvokeCollectionExceptionUPP()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   available as macro/inline
 }
function InvokeCollectionExceptionUPP( c: Collection; status: OSErr; userUPP: CollectionExceptionUPP ): OSErr; external name '_InvokeCollectionExceptionUPP';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)

{*******************************************}
{************ Public interfaces ************}
{*******************************************}
{
 *  NewCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function NewCollection: Collection; external name '_NewCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  DisposeCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
procedure DisposeCollection( c: Collection ); external name '_DisposeCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CloneCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function CloneCollection( c: Collection ): Collection; external name '_CloneCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CountCollectionOwners()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function CountCollectionOwners( c: Collection ): SInt32; external name '_CountCollectionOwners';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  RetainCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function RetainCollection( c: Collection ): OSStatus; external name '_RetainCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  ReleaseCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function ReleaseCollection( c: Collection ): OSStatus; external name '_ReleaseCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetCollectionRetainCount()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.1 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Non-Carbon CFM:   not available
 }
function GetCollectionRetainCount( c: Collection ): ItemCount; external name '_GetCollectionRetainCount';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_1, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CopyCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function CopyCollection( srcCollection: Collection; dstCollection: Collection ): Collection; external name '_CopyCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetCollectionDefaultAttributes()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetCollectionDefaultAttributes( c: Collection ): SInt32; external name '_GetCollectionDefaultAttributes';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetCollectionDefaultAttributes()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
procedure SetCollectionDefaultAttributes( c: Collection; whichAttributes: SInt32; newAttributes: SInt32 ); external name '_SetCollectionDefaultAttributes';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CountCollectionItems()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function CountCollectionItems( c: Collection ): SInt32; external name '_CountCollectionItems';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  AddCollectionItem()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function AddCollectionItem( c: Collection; tag: CollectionTag; id: SInt32; itemSize: SInt32; itemData: {const} UnivPtr ): OSErr; external name '_AddCollectionItem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetCollectionItem()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetCollectionItem( c: Collection; tag: CollectionTag; id: SInt32; var itemSize: SInt32; itemData: UnivPtr ): OSErr; external name '_GetCollectionItem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  RemoveCollectionItem()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function RemoveCollectionItem( c: Collection; tag: CollectionTag; id: SInt32 ): OSErr; external name '_RemoveCollectionItem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetCollectionItemInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function SetCollectionItemInfo( c: Collection; tag: CollectionTag; id: SInt32; whichAttributes: SInt32; newAttributes: SInt32 ): OSErr; external name '_SetCollectionItemInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetCollectionItemInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetCollectionItemInfo( c: Collection; tag: CollectionTag; id: SInt32; var itemIndex: SInt32; var itemSize: SInt32; var attributes: SInt32 ): OSErr; external name '_GetCollectionItemInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  ReplaceIndexedCollectionItem()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function ReplaceIndexedCollectionItem( c: Collection; itemIndex: SInt32; itemSize: SInt32; itemData: {const} UnivPtr ): OSErr; external name '_ReplaceIndexedCollectionItem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetIndexedCollectionItem()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetIndexedCollectionItem( c: Collection; itemIndex: SInt32; var itemSize: SInt32; itemData: UnivPtr ): OSErr; external name '_GetIndexedCollectionItem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  RemoveIndexedCollectionItem()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function RemoveIndexedCollectionItem( c: Collection; itemIndex: SInt32 ): OSErr; external name '_RemoveIndexedCollectionItem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetIndexedCollectionItemInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function SetIndexedCollectionItemInfo( c: Collection; itemIndex: SInt32; whichAttributes: SInt32; newAttributes: SInt32 ): OSErr; external name '_SetIndexedCollectionItemInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetIndexedCollectionItemInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetIndexedCollectionItemInfo( c: Collection; itemIndex: SInt32; var tag: CollectionTag; var id: SInt32; var itemSize: SInt32; var attributes: SInt32 ): OSErr; external name '_GetIndexedCollectionItemInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CollectionTagExists()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function CollectionTagExists( c: Collection; tag: CollectionTag ): Boolean; external name '_CollectionTagExists';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CountCollectionTags()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function CountCollectionTags( c: Collection ): SInt32; external name '_CountCollectionTags';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetIndexedCollectionTag()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetIndexedCollectionTag( c: Collection; tagIndex: SInt32; var tag: CollectionTag ): OSErr; external name '_GetIndexedCollectionTag';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  CountTaggedCollectionItems()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function CountTaggedCollectionItems( c: Collection; tag: CollectionTag ): SInt32; external name '_CountTaggedCollectionItems';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetTaggedCollectionItem()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetTaggedCollectionItem( c: Collection; tag: CollectionTag; whichItem: SInt32; var itemSize: SInt32; itemData: UnivPtr ): OSErr; external name '_GetTaggedCollectionItem';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetTaggedCollectionItemInfo()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetTaggedCollectionItemInfo( c: Collection; tag: CollectionTag; whichItem: SInt32; var id: SInt32; var itemIndex: SInt32; var itemSize: SInt32; var attributes: SInt32 ): OSErr; external name '_GetTaggedCollectionItemInfo';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PurgeCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
procedure PurgeCollection( c: Collection; whichAttributes: SInt32; matchingAttributes: SInt32 ); external name '_PurgeCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  PurgeCollectionTag()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
procedure PurgeCollectionTag( c: Collection; tag: CollectionTag ); external name '_PurgeCollectionTag';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  EmptyCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
procedure EmptyCollection( c: Collection ); external name '_EmptyCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FlattenCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function FlattenCollection( c: Collection; flattenProc: CollectionFlattenUPP; refCon: UnivPtr ): OSErr; external name '_FlattenCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FlattenPartialCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function FlattenPartialCollection( c: Collection; flattenProc: CollectionFlattenUPP; refCon: UnivPtr; whichAttributes: SInt32; matchingAttributes: SInt32 ): OSErr; external name '_FlattenPartialCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  UnflattenCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function UnflattenCollection( c: Collection; flattenProc: CollectionFlattenUPP; refCon: UnivPtr ): OSErr; external name '_UnflattenCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetCollectionExceptionProc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetCollectionExceptionProc( c: Collection ): CollectionExceptionUPP; external name '_GetCollectionExceptionProc';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  SetCollectionExceptionProc()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
procedure SetCollectionExceptionProc( c: Collection; exceptionProc: CollectionExceptionUPP ); external name '_SetCollectionExceptionProc';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetNewCollection()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetNewCollection( collectionID: SInt16 ): Collection; external name '_GetNewCollection';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{********************************************************************}
{************* Utility routines for handle-based access *************}
{********************************************************************}
{
 *  AddCollectionItemHdl()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function AddCollectionItemHdl( aCollection: Collection; tag: CollectionTag; id: SInt32; itemData: Handle ): OSErr; external name '_AddCollectionItemHdl';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetCollectionItemHdl()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetCollectionItemHdl( aCollection: Collection; tag: CollectionTag; id: SInt32; itemData: Handle ): OSErr; external name '_GetCollectionItemHdl';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  ReplaceIndexedCollectionItemHdl()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function ReplaceIndexedCollectionItemHdl( aCollection: Collection; itemIndex: SInt32; itemData: Handle ): OSErr; external name '_ReplaceIndexedCollectionItemHdl';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  GetIndexedCollectionItemHdl()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function GetIndexedCollectionItemHdl( aCollection: Collection; itemIndex: SInt32; itemData: Handle ): OSErr; external name '_GetIndexedCollectionItemHdl';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  FlattenCollectionToHdl()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function FlattenCollectionToHdl( aCollection: Collection; flattened: Handle ): OSErr; external name '_FlattenCollectionToHdl';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{
 *  UnflattenCollectionFromHdl()
 *  
 *  Availability:
 *    Mac OS X:         in version 10.0 and later in CoreServices.framework
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 }
function UnflattenCollectionFromHdl( aCollection: Collection; flattened: Handle ): OSErr; external name '_UnflattenCollectionFromHdl';
(* __OSX_AVAILABLE_BUT_DEPRECATED(__MAC_10_0, __MAC_10_8, __IPHONE_NA, __IPHONE_NA) *)


{$ifc OLDROUTINENAMES}
const
	dontWantTag = kCollectionDontWantTag;
	dontWantId = kCollectionDontWantId;
	dontWantSize = kCollectionDontWantSize;
	dontWantAttributes = kCollectionDontWantAttributes;
	dontWantIndex = kCollectionDontWantIndex;
	dontWantData = kCollectionDontWantData;

const
	noCollectionAttributes = kCollectionNoAttributes;
	allCollectionAttributes = kCollectionAllAttributes;
	userCollectionAttributes = kCollectionUserAttributes;
	defaultCollectionAttributes = kCollectionDefaultAttributes;

const
	collectionUser0Bit = kCollectionUser0Bit;
	collectionUser1Bit = kCollectionUser1Bit;
	collectionUser2Bit = kCollectionUser2Bit;
	collectionUser3Bit = kCollectionUser3Bit;
	collectionUser4Bit = kCollectionUser4Bit;
	collectionUser5Bit = kCollectionUser5Bit;
	collectionUser6Bit = kCollectionUser6Bit;
	collectionUser7Bit = kCollectionUser7Bit;
	collectionUser8Bit = kCollectionUser8Bit;
	collectionUser9Bit = kCollectionUser9Bit;
	collectionUser10Bit = kCollectionUser10Bit;
	collectionUser11Bit = kCollectionUser11Bit;
	collectionUser12Bit = kCollectionUser12Bit;
	collectionUser13Bit = kCollectionUser13Bit;
	collectionUser14Bit = kCollectionUser14Bit;
	collectionUser15Bit = kCollectionUser15Bit;
	collectionReserved0Bit = kCollectionReserved0Bit;
	collectionReserved1Bit = kCollectionReserved1Bit;
	collectionReserved2Bit = kCollectionReserved2Bit;
	collectionReserved3Bit = kCollectionReserved3Bit;
	collectionReserved4Bit = kCollectionReserved4Bit;
	collectionReserved5Bit = kCollectionReserved5Bit;
	collectionReserved6Bit = kCollectionReserved6Bit;
	collectionReserved7Bit = kCollectionReserved7Bit;
	collectionReserved8Bit = kCollectionReserved8Bit;
	collectionReserved9Bit = kCollectionReserved9Bit;
	collectionReserved10Bit = kCollectionReserved10Bit;
	collectionReserved11Bit = kCollectionReserved11Bit;
	collectionReserved12Bit = kCollectionReserved12Bit;
	collectionReserved13Bit = kCollectionReserved13Bit;
	collectionPersistenceBit = kCollectionPersistenceBit;
	collectionLockBit = kCollectionLockBit;

const
	collectionUser0Mask = kCollectionUser0Mask;
	collectionUser1Mask = kCollectionUser1Mask;
	collectionUser2Mask = kCollectionUser2Mask;
	collectionUser3Mask = kCollectionUser3Mask;
	collectionUser4Mask = kCollectionUser4Mask;
	collectionUser5Mask = kCollectionUser5Mask;
	collectionUser6Mask = kCollectionUser6Mask;
	collectionUser7Mask = kCollectionUser7Mask;
	collectionUser8Mask = kCollectionUser8Mask;
	collectionUser9Mask = kCollectionUser9Mask;
	collectionUser10Mask = kCollectionUser10Mask;
	collectionUser11Mask = kCollectionUser11Mask;
	collectionUser12Mask = kCollectionUser12Mask;
	collectionUser13Mask = kCollectionUser13Mask;
	collectionUser14Mask = kCollectionUser14Mask;
	collectionUser15Mask = kCollectionUser15Mask;
	collectionReserved0Mask = kCollectionReserved0Mask;
	collectionReserved1Mask = kCollectionReserved1Mask;
	collectionReserved2Mask = kCollectionReserved2Mask;
	collectionReserved3Mask = kCollectionReserved3Mask;
	collectionReserved4Mask = kCollectionReserved4Mask;
	collectionReserved5Mask = kCollectionReserved5Mask;
	collectionReserved6Mask = kCollectionReserved6Mask;
	collectionReserved7Mask = kCollectionReserved7Mask;
	collectionReserved8Mask = kCollectionReserved8Mask;
	collectionReserved9Mask = kCollectionReserved9Mask;
	collectionReserved10Mask = kCollectionReserved10Mask;
	collectionReserved11Mask = kCollectionReserved11Mask;
	collectionReserved12Mask = kCollectionReserved12Mask;
	collectionReserved13Mask = kCollectionReserved13Mask;
	collectionPersistenceMask = kCollectionPersistenceMask;
	collectionLockMask = kCollectionLockMask;

{$endc}  {OLDROUTINENAMES}

{$endc} {TARGET_OS_MAC}
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
