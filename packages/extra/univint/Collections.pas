{
     File:       Collections.p
 
     Contains:   Collection Manager Interfaces
 
     Version:    Technology: Carbon
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1989-2002 by Apple Computer, Inc., all rights reserved
 
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

unit Collections;
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
uses MacTypes,MixedMode;


{$ALIGN MAC68K}

{***********}
{ Constants }
{***********}
{ Convenience constants for functions which optionally return values }

const
	kCollectionDontWantTag		= 0;
	kCollectionDontWantId		= 0;
	kCollectionDontWantSize		= 0;
	kCollectionDontWantAttributes = 0;
	kCollectionDontWantIndex	= 0;
	kCollectionDontWantData		= 0;


	{	 attributes bits 	}
	kCollectionNoAttributes		= $00000000;					{  no attributes bits set  }
	kCollectionAllAttributes	= $FFFFFFFF;					{  all attributes bits set  }
	kCollectionUserAttributes	= $0000FFFF;					{  user attributes bits  }
	kCollectionDefaultAttributes = $40000000;					{  default attributes - unlocked, persistent  }


	{	 
	    Attribute bits 0 through 15 (entire low word) are reserved for use by the application.
	    Attribute bits 16 through 31 (entire high word) are reserved for use by the Collection Manager.
	    Only bits 31 (kCollectionLockBit) and 30 (kCollectionPersistenceBit) currently have meaning.
		}
	kCollectionUser0Bit			= 0;
	kCollectionUser1Bit			= 1;
	kCollectionUser2Bit			= 2;
	kCollectionUser3Bit			= 3;
	kCollectionUser4Bit			= 4;
	kCollectionUser5Bit			= 5;
	kCollectionUser6Bit			= 6;
	kCollectionUser7Bit			= 7;
	kCollectionUser8Bit			= 8;
	kCollectionUser9Bit			= 9;
	kCollectionUser10Bit		= 10;
	kCollectionUser11Bit		= 11;
	kCollectionUser12Bit		= 12;
	kCollectionUser13Bit		= 13;
	kCollectionUser14Bit		= 14;
	kCollectionUser15Bit		= 15;
	kCollectionReserved0Bit		= 16;
	kCollectionReserved1Bit		= 17;
	kCollectionReserved2Bit		= 18;
	kCollectionReserved3Bit		= 19;
	kCollectionReserved4Bit		= 20;
	kCollectionReserved5Bit		= 21;
	kCollectionReserved6Bit		= 22;
	kCollectionReserved7Bit		= 23;
	kCollectionReserved8Bit		= 24;
	kCollectionReserved9Bit		= 25;
	kCollectionReserved10Bit	= 26;
	kCollectionReserved11Bit	= 27;
	kCollectionReserved12Bit	= 28;
	kCollectionReserved13Bit	= 29;
	kCollectionPersistenceBit	= 30;
	kCollectionLockBit			= 31;


	{	 attribute masks 	}
	kCollectionUser0Mask		= $00000001;
	kCollectionUser1Mask		= $00000002;
	kCollectionUser2Mask		= $00000004;
	kCollectionUser3Mask		= $00000008;
	kCollectionUser4Mask		= $00000010;
	kCollectionUser5Mask		= $00000020;
	kCollectionUser6Mask		= $00000040;
	kCollectionUser7Mask		= $00000080;
	kCollectionUser8Mask		= $00000100;
	kCollectionUser9Mask		= $00000200;
	kCollectionUser10Mask		= $00000400;
	kCollectionUser11Mask		= $00000800;
	kCollectionUser12Mask		= $00001000;
	kCollectionUser13Mask		= $00002000;
	kCollectionUser14Mask		= $00004000;
	kCollectionUser15Mask		= $00008000;
	kCollectionReserved0Mask	= $00010000;
	kCollectionReserved1Mask	= $00020000;
	kCollectionReserved2Mask	= $00040000;
	kCollectionReserved3Mask	= $00080000;
	kCollectionReserved4Mask	= $00100000;
	kCollectionReserved5Mask	= $00200000;
	kCollectionReserved6Mask	= $00400000;
	kCollectionReserved7Mask	= $00800000;
	kCollectionReserved8Mask	= $01000000;
	kCollectionReserved9Mask	= $02000000;
	kCollectionReserved10Mask	= $04000000;
	kCollectionReserved11Mask	= $08000000;
	kCollectionReserved12Mask	= $10000000;
	kCollectionReserved13Mask	= $20000000;
	kCollectionPersistenceMask	= $40000000;
	kCollectionLockMask			= $80000000;


	{	*********	}
	{	 Types   	}
	{	*********	}
	{	 abstract data type for a collection 	}

type
	Collection    = ^SInt32; { an opaque 32-bit type }
	CollectionPtr = ^Collection;  { when a var xx:Collection parameter can be nil, it is changed to xx: CollectionPtr }
	{	 collection member 4 byte tag 	}
	CollectionTag						= FourCharCode;
{$ifc TYPED_FUNCTION_POINTERS}
	CollectionFlattenProcPtr = function(size: SInt32; data: UnivPtr; refCon: UnivPtr): OSErr;
{$elsec}
	CollectionFlattenProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	CollectionExceptionProcPtr = function(c: Collection; status: OSErr): OSErr;
{$elsec}
	CollectionExceptionProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	CollectionFlattenUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CollectionFlattenUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	CollectionExceptionUPP = ^SInt32; { an opaque UPP }
{$elsec}
	CollectionExceptionUPP = UniversalProcPtr;
{$endc}	

const
	uppCollectionFlattenProcInfo = $00000FE0;
	uppCollectionExceptionProcInfo = $000002E0;
	{
	 *  NewCollectionFlattenUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewCollectionFlattenUPP(userRoutine: CollectionFlattenProcPtr): CollectionFlattenUPP; external name '_NewCollectionFlattenUPP'; { old name was NewCollectionFlattenProc }
{
 *  NewCollectionExceptionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewCollectionExceptionUPP(userRoutine: CollectionExceptionProcPtr): CollectionExceptionUPP; external name '_NewCollectionExceptionUPP'; { old name was NewCollectionExceptionProc }
{
 *  DisposeCollectionFlattenUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeCollectionFlattenUPP(userUPP: CollectionFlattenUPP); external name '_DisposeCollectionFlattenUPP';
{
 *  DisposeCollectionExceptionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeCollectionExceptionUPP(userUPP: CollectionExceptionUPP); external name '_DisposeCollectionExceptionUPP';
{
 *  InvokeCollectionFlattenUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeCollectionFlattenUPP(size: SInt32; data: UnivPtr; refCon: UnivPtr; userRoutine: CollectionFlattenUPP): OSErr; external name '_InvokeCollectionFlattenUPP'; { old name was CallCollectionFlattenProc }
{
 *  InvokeCollectionExceptionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeCollectionExceptionUPP(c: Collection; status: OSErr; userRoutine: CollectionExceptionUPP): OSErr; external name '_InvokeCollectionExceptionUPP'; { old name was CallCollectionExceptionProc }
{*******************************************}
{************ Public interfaces ************}
{*******************************************}
{
 *  NewCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewCollection: Collection; external name '_NewCollection';
{
 *  DisposeCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeCollection(c: Collection); external name '_DisposeCollection';
{
 *  CloneCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CloneCollection(c: Collection): Collection; external name '_CloneCollection';
{
 *  CountCollectionOwners()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CountCollectionOwners(c: Collection): SInt32; external name '_CountCollectionOwners';
{
 *  RetainCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RetainCollection(c: Collection): OSStatus; external name '_RetainCollection';


{
 *  ReleaseCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ReleaseCollection(c: Collection): OSStatus; external name '_ReleaseCollection';


{
 *  GetCollectionRetainCount()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetCollectionRetainCount(c: Collection): ItemCount; external name '_GetCollectionRetainCount';


{
 *  CopyCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CopyCollection(srcCollection: Collection; dstCollection: Collection): Collection; external name '_CopyCollection';
{
 *  GetCollectionDefaultAttributes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetCollectionDefaultAttributes(c: Collection): SInt32; external name '_GetCollectionDefaultAttributes';
{
 *  SetCollectionDefaultAttributes()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetCollectionDefaultAttributes(c: Collection; whichAttributes: SInt32; newAttributes: SInt32); external name '_SetCollectionDefaultAttributes';
{
 *  CountCollectionItems()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CountCollectionItems(c: Collection): SInt32; external name '_CountCollectionItems';
{
 *  AddCollectionItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AddCollectionItem(c: Collection; tag: CollectionTag; id: SInt32; itemSize: SInt32; itemData: UnivPtr): OSErr; external name '_AddCollectionItem';
{
 *  GetCollectionItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetCollectionItem(c: Collection; tag: CollectionTag; id: SInt32; var itemSize: SInt32; itemData: UnivPtr): OSErr; external name '_GetCollectionItem';
{
 *  RemoveCollectionItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RemoveCollectionItem(c: Collection; tag: CollectionTag; id: SInt32): OSErr; external name '_RemoveCollectionItem';
{
 *  SetCollectionItemInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetCollectionItemInfo(c: Collection; tag: CollectionTag; id: SInt32; whichAttributes: SInt32; newAttributes: SInt32): OSErr; external name '_SetCollectionItemInfo';
{
 *  GetCollectionItemInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetCollectionItemInfo(c: Collection; tag: CollectionTag; id: SInt32; var index: SInt32; var itemSize: SInt32; var attributes: SInt32): OSErr; external name '_GetCollectionItemInfo';
{
 *  ReplaceIndexedCollectionItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ReplaceIndexedCollectionItem(c: Collection; index: SInt32; itemSize: SInt32; itemData: UnivPtr): OSErr; external name '_ReplaceIndexedCollectionItem';
{
 *  GetIndexedCollectionItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIndexedCollectionItem(c: Collection; index: SInt32; var itemSize: SInt32; itemData: UnivPtr): OSErr; external name '_GetIndexedCollectionItem';
{
 *  RemoveIndexedCollectionItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function RemoveIndexedCollectionItem(c: Collection; index: SInt32): OSErr; external name '_RemoveIndexedCollectionItem';
{
 *  SetIndexedCollectionItemInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function SetIndexedCollectionItemInfo(c: Collection; index: SInt32; whichAttributes: SInt32; newAttributes: SInt32): OSErr; external name '_SetIndexedCollectionItemInfo';
{
 *  GetIndexedCollectionItemInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIndexedCollectionItemInfo(c: Collection; index: SInt32; var tag: CollectionTag; var id: SInt32; var itemSize: SInt32; var attributes: SInt32): OSErr; external name '_GetIndexedCollectionItemInfo';
{
 *  CollectionTagExists()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CollectionTagExists(c: Collection; tag: CollectionTag): boolean; external name '_CollectionTagExists';
{
 *  CountCollectionTags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CountCollectionTags(c: Collection): SInt32; external name '_CountCollectionTags';
{
 *  GetIndexedCollectionTag()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIndexedCollectionTag(c: Collection; tagIndex: SInt32; var tag: CollectionTag): OSErr; external name '_GetIndexedCollectionTag';
{
 *  CountTaggedCollectionItems()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CountTaggedCollectionItems(c: Collection; tag: CollectionTag): SInt32; external name '_CountTaggedCollectionItems';
{
 *  GetTaggedCollectionItem()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetTaggedCollectionItem(c: Collection; tag: CollectionTag; whichItem: SInt32; var itemSize: SInt32; itemData: UnivPtr): OSErr; external name '_GetTaggedCollectionItem';
{
 *  GetTaggedCollectionItemInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetTaggedCollectionItemInfo(c: Collection; tag: CollectionTag; whichItem: SInt32; var id: SInt32; var index: SInt32; var itemSize: SInt32; var attributes: SInt32): OSErr; external name '_GetTaggedCollectionItemInfo';
{
 *  PurgeCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PurgeCollection(c: Collection; whichAttributes: SInt32; matchingAttributes: SInt32); external name '_PurgeCollection';
{
 *  PurgeCollectionTag()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure PurgeCollectionTag(c: Collection; tag: CollectionTag); external name '_PurgeCollectionTag';
{
 *  EmptyCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure EmptyCollection(c: Collection); external name '_EmptyCollection';
{
 *  FlattenCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FlattenCollection(c: Collection; flattenProc: CollectionFlattenUPP; refCon: UnivPtr): OSErr; external name '_FlattenCollection';
{
 *  FlattenPartialCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FlattenPartialCollection(c: Collection; flattenProc: CollectionFlattenUPP; refCon: UnivPtr; whichAttributes: SInt32; matchingAttributes: SInt32): OSErr; external name '_FlattenPartialCollection';
{
 *  UnflattenCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UnflattenCollection(c: Collection; flattenProc: CollectionFlattenUPP; refCon: UnivPtr): OSErr; external name '_UnflattenCollection';
{
 *  GetCollectionExceptionProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetCollectionExceptionProc(c: Collection): CollectionExceptionUPP; external name '_GetCollectionExceptionProc';
{
 *  SetCollectionExceptionProc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SetCollectionExceptionProc(c: Collection; exceptionProc: CollectionExceptionUPP); external name '_SetCollectionExceptionProc';
{
 *  GetNewCollection()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetNewCollection(collectionID: SInt16): Collection; external name '_GetNewCollection';
{********************************************************************}
{************* Utility routines for handle-based access *************}
{********************************************************************}
{
 *  AddCollectionItemHdl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function AddCollectionItemHdl(aCollection: Collection; tag: CollectionTag; id: SInt32; itemData: Handle): OSErr; external name '_AddCollectionItemHdl';
{
 *  GetCollectionItemHdl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetCollectionItemHdl(aCollection: Collection; tag: CollectionTag; id: SInt32; itemData: Handle): OSErr; external name '_GetCollectionItemHdl';
{
 *  ReplaceIndexedCollectionItemHdl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ReplaceIndexedCollectionItemHdl(aCollection: Collection; index: SInt32; itemData: Handle): OSErr; external name '_ReplaceIndexedCollectionItemHdl';
{
 *  GetIndexedCollectionItemHdl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetIndexedCollectionItemHdl(aCollection: Collection; index: SInt32; itemData: Handle): OSErr; external name '_GetIndexedCollectionItemHdl';
{
 *  FlattenCollectionToHdl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FlattenCollectionToHdl(aCollection: Collection; flattened: Handle): OSErr; external name '_FlattenCollectionToHdl';
{
 *  UnflattenCollectionFromHdl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CollectionsLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UnflattenCollectionFromHdl(aCollection: Collection; flattened: Handle): OSErr; external name '_UnflattenCollectionFromHdl';
{$ifc OLDROUTINENAMES}

const
	dontWantTag					= 0;
	dontWantId					= 0;
	dontWantSize				= 0;
	dontWantAttributes			= 0;
	dontWantIndex				= 0;
	dontWantData				= 0;

	noCollectionAttributes		= $00000000;
	allCollectionAttributes		= $FFFFFFFF;
	userCollectionAttributes	= $0000FFFF;
	defaultCollectionAttributes	= $40000000;

	collectionUser0Bit			= 0;
	collectionUser1Bit			= 1;
	collectionUser2Bit			= 2;
	collectionUser3Bit			= 3;
	collectionUser4Bit			= 4;
	collectionUser5Bit			= 5;
	collectionUser6Bit			= 6;
	collectionUser7Bit			= 7;
	collectionUser8Bit			= 8;
	collectionUser9Bit			= 9;
	collectionUser10Bit			= 10;
	collectionUser11Bit			= 11;
	collectionUser12Bit			= 12;
	collectionUser13Bit			= 13;
	collectionUser14Bit			= 14;
	collectionUser15Bit			= 15;
	collectionReserved0Bit		= 16;
	collectionReserved1Bit		= 17;
	collectionReserved2Bit		= 18;
	collectionReserved3Bit		= 19;
	collectionReserved4Bit		= 20;
	collectionReserved5Bit		= 21;
	collectionReserved6Bit		= 22;
	collectionReserved7Bit		= 23;
	collectionReserved8Bit		= 24;
	collectionReserved9Bit		= 25;
	collectionReserved10Bit		= 26;
	collectionReserved11Bit		= 27;
	collectionReserved12Bit		= 28;
	collectionReserved13Bit		= 29;
	collectionPersistenceBit	= 30;
	collectionLockBit			= 31;

	collectionUser0Mask			= $00000001;
	collectionUser1Mask			= $00000002;
	collectionUser2Mask			= $00000004;
	collectionUser3Mask			= $00000008;
	collectionUser4Mask			= $00000010;
	collectionUser5Mask			= $00000020;
	collectionUser6Mask			= $00000040;
	collectionUser7Mask			= $00000080;
	collectionUser8Mask			= $00000100;
	collectionUser9Mask			= $00000200;
	collectionUser10Mask		= $00000400;
	collectionUser11Mask		= $00000800;
	collectionUser12Mask		= $00001000;
	collectionUser13Mask		= $00002000;
	collectionUser14Mask		= $00004000;
	collectionUser15Mask		= $00008000;
	collectionReserved0Mask		= $00010000;
	collectionReserved1Mask		= $00020000;
	collectionReserved2Mask		= $00040000;
	collectionReserved3Mask		= $00080000;
	collectionReserved4Mask		= $00100000;
	collectionReserved5Mask		= $00200000;
	collectionReserved6Mask		= $00400000;
	collectionReserved7Mask		= $00800000;
	collectionReserved8Mask		= $01000000;
	collectionReserved9Mask		= $02000000;
	collectionReserved10Mask	= $04000000;
	collectionReserved11Mask	= $08000000;
	collectionReserved12Mask	= $10000000;
	collectionReserved13Mask	= $20000000;
	collectionPersistenceMask	= $40000000;
	collectionLockMask			= $80000000;

{$endc}  {OLDROUTINENAMES}


{$ALIGN MAC68K}


end.
