{
     File:       NameRegistry.p
 
     Contains:   NameRegistry Interfaces
 
     Version:    Technology: MacOS
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1993-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Pascal Translation Updated:  Gale R Paeper, <gpaeper@empirenet.com>, 2006 }

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

unit NameRegistry;
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
uses MacTypes;


{$ALIGN POWER}

{******************************************************************************
 * 
 * Foundation Types
 *
 }
{ Value of a property }

type
	RegPropertyValue					= Ptr;
	{	 Length of property value 	}
	RegPropertyValueSize				= UInt32;
	{	******************************************************************************
	 * 
	 * RegEntryID   :   The Global x-Namespace Entry Identifier
	 *
	 	}
	RegEntryIDPtr = ^RegEntryID;
	RegEntryID = record
		contents:				array [0..3] of UInt32;
	end;

	{	******************************************************************************
	 *
	 * Root Entry Name Definitions  (Applies to all Names in the RootNameSpace)
	 *
	 *  ¥ Names are a colon-separated list of name components.  Name components
	 *    may not themselves contain colons.  
	 *  ¥ Names are presented as null-terminated ASCII character strings.
	 *  ¥ Names follow similar parsing rules to Apple file system absolute
	 *    and relative paths.  However the '::' parent directory syntax is
	 *    not currently supported.
	 	}
	{	 Max length of Entry Name 	}

const
	kRegCStrMaxEntryNameLength	= 47;

	{	 Entry Names are single byte ASCII 	}

type
	RegCStrEntryName					= char;
	RegCStrEntryNamePtr					= CStringPtr;
	{  length of RegCStrEntryNameBuf =  kRegCStrMaxEntryNameLength+1 }
	RegCStrEntryNameBuf					= packed array [0..47] of char;
	RegCStrPathName					    = char;
	RegPathNameSize						= UInt32;
	RegCStrPathNamePtr                  = CStringPtr;

const
	kRegPathNameSeparator		= 58;							{  0x3A  }
	kRegEntryNameTerminator		= $00;							{  '\0'  }
	kRegPathNameTerminator		= $00;							{  '\0'  }

	{	******************************************************************************
	 *
	 * Property Name and ID Definitions
	 *  (Applies to all Properties Regardless of NameSpace)
	 	}
	kRegMaximumPropertyNameLength = 31;							{  Max length of Property Name  }
	kRegPropertyNameTerminator	= $00;							{  '\0'  }


type
	RegPropertyNameBuf					= packed array [0..31] of char;
	RegPropertyName					    = char;
	RegPropertyNamePtr					= CStringPtr;
	{	******************************************************************************
	 *
	 * Iteration Operations
	 *
	 *  These specify direction when traversing the name relationships
	 	}
	RegIterationOp						= UInt32;
	RegEntryIterationOp					= RegIterationOp;

const
																{  Absolute locations }
	kRegIterRoot				= $00000002;					{  "Upward" Relationships  }
	kRegIterParents				= $00000003;					{  include all  parent(s) of entry  }
																{  "Downward" Relationships }
	kRegIterChildren			= $00000004;					{  include all children  }
	kRegIterSubTrees			= $00000005;					{  include all sub trees of entry  }
	kRegIterDescendants			= $00000005;					{  include all descendants of entry  }
																{  "Horizontal" Relationships  }
	kRegIterSibling				= $00000006;					{  include all siblings  }
																{  Keep doing the same thing }
	kRegIterContinue			= $00000001;

	{	******************************************************************************
	 *
	 * Name Entry and Property Modifiers
	 *
	 *
	 *
	 * Modifiers describe special characteristics of names
	 * and properties.  Modifiers might be supported for
	 * some names and not others.
	 * 
	 * Device Drivers should not rely on functionality
	 * specified as a modifier.
	 	}

type
	RegModifiers						= UInt32;
	RegEntryModifiers					= RegModifiers;
	RegPropertyModifiers				= RegModifiers;

const
	kRegNoModifiers				= $00000000;					{  no entry modifiers in place  }
	kRegUniversalModifierMask	= $0000FFFF;					{  mods to all entries  }
	kRegNameSpaceModifierMask	= $00FF0000;					{  mods to all entries within namespace  }
	kRegModifierMask			= $FF000000;					{  mods to just this entry  }

	{	 Universal Property Modifiers 	}
	kRegPropertyValueIsSavedToNVRAM = $00000020;				{  property is non-volatile (saved in NVRAM)  }
	kRegPropertyValueIsSavedToDisk = $00000040;					{  property is non-volatile (saved on disk)  }

	{	 NameRegistry version, Gestalt/PEF-style -- MUST BE KEPT IN SYNC WITH MAKEFILE !! 	}
	LatestNR_PEFVersion			= $01030000;					{  latest NameRegistryLib version (Gestalt/PEF-style)  }

	{	 ///////////////////////
	//
	// The Registry API
	//
	/////////////////////// 	}
	{	 NameRegistry dispatch indexes 	}
	kSelectRegistryEntryIDInit	= 0;
	kSelectRegistryEntryIDCompare = 1;
	kSelectRegistryEntryIDCopy	= 2;
	kSelectRegistryEntryIDDispose = 3;
	kSelectRegistryCStrEntryCreate = 4;
	kSelectRegistryEntryDelete	= 5;
	kSelectRegistryEntryCopy	= 6;
	kSelectRegistryEntryIterateCreate = 7;
	kSelectRegistryEntryIterateDispose = 8;
	kSelectRegistryEntryIterateSet = 9;
	kSelectRegistryEntryIterate	= 10;
	kSelectRegistryEntrySearch	= 11;
	kSelectRegistryCStrEntryLookup = 12;
	kSelectRegistryEntryToPathSize = 13;
	kSelectRegistryCStrEntryToPath = 14;
	kSelectRegistryCStrEntryToName = 15;
	kSelectRegistryPropertyCreate = 16;
	kSelectRegistryPropertyDelete = 17;
	kSelectRegistryPropertyRename = 18;
	kSelectRegistryPropertyIterateCreate = 19;
	kSelectRegistryPropertyIterateDispose = 20;
	kSelectRegistryPropertyIterate = 21;
	kSelectRegistryPropertyGetSize = 22;
	kSelectRegistryPropertyGet	= 23;
	kSelectRegistryPropertySet	= 24;
	kSelectRegistryEntryGetMod	= 25;
	kSelectRegistryEntrySetMod	= 26;
	kSelectRegistryPropertyGetMod = 27;
	kSelectRegistryPropertySetMod = 28;
	kSelectRegistryEntryMod		= 29;
	kSelectRegistryEntryPropertyMod = 30;						{  if you add more selectors here, remember to change 'kSelectRegistryHighestSelector' below }
	kSelectRegistryHighestSelector = 30;


	{	 ///////////////////////
	//
	// Entry Management
	//
	/////////////////////// 	}

	{	-------------------------------
	 * EntryID handling
	 	}
	{	
	 * Initialize an EntryID to a known invalid state
	 *   note: invalid != uninitialized
	 	}
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  RegistryEntryIDInit()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function RegistryEntryIDInit(var id: RegEntryID): OSStatus; external name '_RegistryEntryIDInit';
{
 * Compare EntryID's for equality or if invalid
 *
 * If a NULL value is given for either id1 or id2, the other id 
 * is compared with an invalid ID.  If both are NULL, the id's 
 * are consided equal (result = true). 
 }
{
 *  RegistryEntryIDCompare()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryIDCompare(const (*var*) id1: RegEntryID; const (*var*) id2: RegEntryID): boolean; external name '_RegistryEntryIDCompare';
{
 * Copy an EntryID
 }
{
 *  RegistryEntryIDCopy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryIDCopy(const (*var*) src: RegEntryID; var dst: RegEntryID): OSStatus; external name '_RegistryEntryIDCopy';
{
 * Free an ID so it can be reused.
 }
{
 *  RegistryEntryIDDispose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryIDDispose(var id: RegEntryID): OSStatus; external name '_RegistryEntryIDDispose';
{-------------------------------
 * Adding and removing entries
 *
 * If (parentEntry) is NULL, the name is assumed
 * to be a rooted path. It is rooted to an anonymous, unnamed root.
 }
{
 *  RegistryCStrEntryCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryCStrEntryCreate(const (*var*) parentEntry: RegEntryID; {const} name: {variable-size-array} RegCStrPathNamePtr; var newEntry: RegEntryID): OSStatus; external name '_RegistryCStrEntryCreate';
{
 *  RegistryEntryDelete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryDelete(const (*var*) id: RegEntryID): OSStatus; external name '_RegistryEntryDelete';
{
 *  RegistryEntryCopy()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryCopy(var parentEntryID: RegEntryID; var sourceDevice: RegEntryID; var destDevice: RegEntryID): OSStatus; external name '_RegistryEntryCopy';
{---------------------------
 * Traversing the namespace
 *
 * To support arbitrary namespace implementations in the future,
 * I have hidden the form that the place pointer takes.  The previous
 * interface exposed the place pointer by specifying it as a
 * RegEntryID.
 *
 * I have also removed any notion of returning the entries
 * in a particular order, because an implementation might
 * return the names in semi-random order.  Many name service
 * implementations will store the names in a hashed lookup
 * table.
 *
 * Writing code to traverse some set of names consists of
 * a call to begin the iteration, the iteration loop, and
 * a call to end the iteration.  The begin call initializes
 * the iteration cookie data structure.  The call to end the 
 * iteration should be called even in the case of error so 
 * that allocated data structures can be freed.
 *
 *  Create(...)
 *  do (
 *      Iterate(...);
 *  ) while (!done);
 *  Dispose(...);
 *
 * This is the basic code structure for callers of the iteration
 * interface.
 }
{$endc}  {CALL_NOT_IN_CARBON}


type
	RegEntryIter    = ^SInt32; { an opaque 32-bit type }
	RegEntryIterPtr = ^RegEntryIter;  { when a var xx:RegEntryIter parameter can be nil, it is changed to xx: RegEntryIterPtr }
	{	 
	 * create/dispose the iterator structure
	 *   defaults to root with relationship = kRegIterDescendants
	 	}
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  RegistryEntryIterateCreate()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function RegistryEntryIterateCreate(var cookie: RegEntryIter): OSStatus; external name '_RegistryEntryIterateCreate';
{
 *  RegistryEntryIterateDispose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryIterateDispose(var cookie: RegEntryIter): OSStatus; external name '_RegistryEntryIterateDispose';
{ 
 * set Entry Iterator to specified entry
 }
{
 *  RegistryEntryIterateSet()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryIterateSet(var cookie: RegEntryIter; const (*var*) startEntryID: RegEntryID): OSStatus; external name '_RegistryEntryIterateSet';
{
 * Return each value of the iteration
 *
 * return entries related to the current entry
 * with the specified relationship
 }
{
 *  RegistryEntryIterate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryIterate(var cookie: RegEntryIter; relationship: RegEntryIterationOp; var foundEntry: RegEntryID; var done: boolean): OSStatus; external name '_RegistryEntryIterate';
{
 * return entries with the specified property
 *
 * A NULL RegPropertyValue pointer will return an
 * entry with the property containing any value.
 }
{
 *  RegistryEntrySearch()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntrySearch(var cookie: RegEntryIter; relationship: RegEntryIterationOp; var foundEntry: RegEntryID; var done: boolean; {const} propertyName: {variable-size-array} RegPropertyNamePtr; propertyValue: UnivPtr; propertySize: RegPropertyValueSize): OSStatus; external name '_RegistryEntrySearch';
{--------------------------------
 * Find a name in the namespace
 *
 * This is the fast lookup mechanism.
 * NOTE:  A reverse lookup mechanism
 *    has not been provided because
 *        some name services may not
 *        provide a fast, general reverse
 *        lookup.
 }
{
 *  RegistryCStrEntryLookup()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryCStrEntryLookup(const (*var*) searchPointID: RegEntryID; {const} pathName: {variable-size-array} RegCStrPathNamePtr; var foundEntry: RegEntryID): OSStatus; external name '_RegistryCStrEntryLookup';
{---------------------------------------------
 * Convert an entry to a rooted name string
 *
 * A utility routine to turn an Entry ID
 * back into a name string.
 }
{
 *  RegistryEntryToPathSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryToPathSize(const (*var*) entryID: RegEntryID; var pathSize: RegPathNameSize): OSStatus; external name '_RegistryEntryToPathSize';
{
 *  RegistryCStrEntryToPath()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryCStrEntryToPath(const (*var*) entryID: RegEntryID; var pathName: RegCStrPathName; pathSize: RegPathNameSize): OSStatus; external name '_RegistryCStrEntryToPath';
{
 * Parse a path name.
 *
 * Retrieve the last component of the path, and
 * return a spec for the parent.
 }
{
 *  RegistryCStrEntryToName()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryCStrEntryToName(const (*var*) entryID: RegEntryID; var parentEntry: RegEntryID; var nameComponent: RegCStrEntryName; var done: boolean): OSStatus; external name '_RegistryCStrEntryToName';
{ //////////////////////////////////////////////////////
//
// Property Management
//
////////////////////////////////////////////////////// }
{-------------------------------
 * Adding and removing properties
 }
{
 *  RegistryPropertyCreate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryPropertyCreate(const (*var*) entryID: RegEntryID; {const} propertyName: {variable-size-array} RegPropertyNamePtr; propertyValue: UnivPtr; propertySize: RegPropertyValueSize): OSStatus; external name '_RegistryPropertyCreate';
{
 *  RegistryPropertyDelete()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryPropertyDelete(const (*var*) entryID: RegEntryID; {const} propertyName: {variable-size-array} RegPropertyNamePtr): OSStatus; external name '_RegistryPropertyDelete';
{
 *  RegistryPropertyRename()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryPropertyRename(const (*var*) entry: RegEntryID; {const} oldName: {variable-size-array} RegPropertyNamePtr; {const} newName: {variable-size-array} RegPropertyNamePtr): OSStatus; external name '_RegistryPropertyRename';
{---------------------------
 * Traversing the Properties of a name
 *
 }
{$endc}  {CALL_NOT_IN_CARBON}


type
	RegPropertyIter    = ^SInt32; { an opaque 32-bit type }
	RegPropertyIterPtr = ^RegPropertyIter;  { when a var xx:RegPropertyIter parameter can be nil, it is changed to xx: RegPropertyIterPtr }
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  RegistryPropertyIterateCreate()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function RegistryPropertyIterateCreate(const (*var*) entry: RegEntryID; var cookie: RegPropertyIter): OSStatus; external name '_RegistryPropertyIterateCreate';
{
 *  RegistryPropertyIterateDispose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryPropertyIterateDispose(var cookie: RegPropertyIter): OSStatus; external name '_RegistryPropertyIterateDispose';
{
 *  RegistryPropertyIterate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryPropertyIterate(var cookie: RegPropertyIter; var foundProperty: RegPropertyName; var done: boolean): OSStatus; external name '_RegistryPropertyIterate';
{
 * Get the value of the specified property for the specified entry.
 *
 }
{
 *  RegistryPropertyGetSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryPropertyGetSize(const (*var*) entryID: RegEntryID; {const} propertyName: {variable-size-array} RegPropertyNamePtr; var propertySize: RegPropertyValueSize): OSStatus; external name '_RegistryPropertyGetSize';
{
 * (*propertySize) is the maximum size of the value returned in the buffer
 * pointed to by (propertyValue).  Upon return, (*propertySize) is the size of the
 * value returned.
 }
{
 *  RegistryPropertyGet()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryPropertyGet(const (*var*) entryID: RegEntryID; {const} propertyName: {variable-size-array} RegPropertyNamePtr; propertyValue: UnivPtr; var propertySize: RegPropertyValueSize): OSStatus; external name '_RegistryPropertyGet';
{
 *  RegistryPropertySet()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryPropertySet(const (*var*) entryID: RegEntryID; {const} propertyName: {variable-size-array} RegPropertyNamePtr; propertyValue: UnivPtr; propertySize: RegPropertyValueSize): OSStatus; external name '_RegistryPropertySet';
{ //////////////////////////////////////////////////////
//
// Modifier Management
//
////////////////////////////////////////////////////// }
{
 * Modifiers describe special characteristics of names
 * and properties.  Modifiers might be supported for
 * some names and not others.
 * 
 * Device Drivers should not rely on functionality
 * specified as a modifier.  These interfaces
 * are for use in writing Experts.
 }
{
 * Get and Set operators for entry modifiers
 }
{
 *  RegistryEntryGetMod()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryGetMod(const (*var*) entry: RegEntryID; var modifiers: RegEntryModifiers): OSStatus; external name '_RegistryEntryGetMod';
{
 *  RegistryEntrySetMod()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntrySetMod(const (*var*) entry: RegEntryID; modifiers: RegEntryModifiers): OSStatus; external name '_RegistryEntrySetMod';
{
 * Get and Set operators for property modifiers
 }
{
 *  RegistryPropertyGetMod()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryPropertyGetMod(const (*var*) entry: RegEntryID; {const} name: {variable-size-array} RegPropertyNamePtr; var modifiers: RegPropertyModifiers): OSStatus; external name '_RegistryPropertyGetMod';
{
 *  RegistryPropertySetMod()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryPropertySetMod(const (*var*) entry: RegEntryID; {const} name: {variable-size-array} RegPropertyNamePtr; modifiers: RegPropertyModifiers): OSStatus; external name '_RegistryPropertySetMod';
{
 * Iterator operator for entry modifier search
 }
{
 *  RegistryEntryMod()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryMod(var cookie: RegEntryIter; relationship: RegEntryIterationOp; var foundEntry: RegEntryID; var done: boolean; matchingModifiers: RegEntryModifiers): OSStatus; external name '_RegistryEntryMod';
{
 * Iterator operator for entries with matching 
 * property modifiers
 }
{
 *  RegistryEntryPropertyMod()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NameRegistryLib 1.0 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function RegistryEntryPropertyMod(var cookie: RegEntryIter; relationship: RegEntryIterationOp; var foundEntry: RegEntryID; var done: boolean; matchingModifiers: RegPropertyModifiers): OSStatus; external name '_RegistryEntryPropertyMod';

{$endc}  {CALL_NOT_IN_CARBON}

{$ALIGN MAC68K}


end.
