{
     File:       OSServices/IconStorage.h
 
     Contains:   Services to load and share icon family data.
 
     Version:    OSServices-352~2
 
     Copyright:  © 2000-2008 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}
{       Initial Pascal Translation:  Gorazd Krosl, <gorazd_1957@yahoo.ca>, October 2009 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit IconStorage;
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
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
{$ifc defined(iphonesim)}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{ will require compiler define when/if other Apple devices with ARM cpus ship }
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ is defined.}
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

{$ALIGN MAC68K}

{ virtual ARGB icon types. Each type will be split into separate 24 bit RGB data and 8 bit mask for storage in icns container }
{ the ARGB bitmap must be non-premultiplied }
const
	kIconServices16PixelDataARGB = FourCharCode('ic04'); { uses kSmall32BitIconIndex and kSmallDeepMaskIconIndex}
	kIconServices32PixelDataARGB = FourCharCode('ic05'); { uses kLarge32BitIconIndex and kLargeDeepMaskIconIndex}
	kIconServices48PixelDataARGB = FourCharCode('ic06'); { uses kHuge32BitIconIndex and kHugeDeepMaskIconIndex}
	kIconServices128PixelDataARGB = FourCharCode('ic07'); { uses kThumbnailDataIndex and kThumbnailMaskIndex}

{ The following icon types can only be used as an icon element }
{ inside a 'icns' icon family }
const
	kIconServices256PixelDataARGB = FourCharCode('ic08'); { non-premultiplied 256x256 ARGB bitmap}
	kIconServices512PixelDataARGB = FourCharCode('ic09'); { non-premultiplied 512x512 ARGB bitmap}
	kThumbnail32BitData = FourCharCode('it32');
	kThumbnail8BitMask = FourCharCode('t8mk');

const
	kHuge1BitMask = FourCharCode('ich#');
	kHuge4BitData = FourCharCode('ich4');
	kHuge8BitData = FourCharCode('ich8');
	kHuge32BitData = FourCharCode('ih32');
	kHuge8BitMask = FourCharCode('h8mk');

{ The following icon types can be used as a resource type }
{ or as an icon element type inside a 'icns' icon family }
const
	kLarge1BitMask = FourCharCode('ICN#');
	kLarge4BitData = FourCharCode('icl4');
	kLarge8BitData = FourCharCode('icl8');
	kLarge32BitData = FourCharCode('il32');
	kLarge8BitMask = FourCharCode('l8mk');
	kSmall1BitMask = FourCharCode('ics#');
	kSmall4BitData = FourCharCode('ics4');
	kSmall8BitData = FourCharCode('ics8');
	kSmall32BitData = FourCharCode('is32');
	kSmall8BitMask = FourCharCode('s8mk');
	kMini1BitMask = FourCharCode('icm#');
	kMini4BitData = FourCharCode('icm4');
	kMini8BitData = FourCharCode('icm8');

{ Obsolete. Use names defined above. }
const
	large1BitMask = kLarge1BitMask;
	large4BitData = kLarge4BitData;
	large8BitData = kLarge8BitData;
	small1BitMask = kSmall1BitMask;
	small4BitData = kSmall4BitData;
	small8BitData = kSmall8BitData;
	mini1BitMask = kMini1BitMask;
	mini4BitData = kMini4BitData;
	mini8BitData = kMini8BitData;

{
    IconFamily 'icns' resources contain an entire IconFamily (all sizes and depths).  
   For custom icons, icns IconFamily resources of the custom icon resource ID are fetched first before
   the classic custom icons (individual 'ics#, ICN#, etc) are fetched.  If the fetch of the icns resource
   succeeds then the icns is looked at exclusively for the icon data.
   For custom icons, new icon features such as 32-bit deep icons are only fetched from the icns resource.
   This is to avoid incompatibilities with cut & paste of new style icons with an older version of the
   MacOS Finder.
   DriverGestalt is called with code kdgMediaIconSuite by IconServices after calling FSM to determine a
   driver icon for a particular device.  The result of the kdgMediaIconSuite call to DriverGestalt should
   be a pointer an an IconFamily.  In this manner driver vendors can provide rich, detailed drive icons
   instead of the 1-bit variety previously supported.
   The IconFamilyElement and IconFamilyResource data types (which also include the data types
   IconFamilyPtr and IconFamilyHandle) are always big-endian.
}

const
	kIconFamilyType = FourCharCode('icns');


type
	IconFamilyElement = record
		elementType: OSType;            { 'ICN#', 'icl8', etc...}
		elementSize: SInt32;            { Size of this element}
		elementData : packed array [0..0] of UInt8;
	end;
	IconFamilyElementPtr = ^IconFamilyElement;
type
	IconFamilyResource = record
		resourceType: OSType;           { Always 'icns'}
		resourceSize: SInt32;           { Total size of this resource}
		elements: array[0..0] of IconFamilyElement;
	end;
	IconFamilyResourcePtr = ^IconFamilyResource;
type
	IconFamilyPtr = IconFamilyResourcePtr;
	IconFamilyPtrPtr = ^IconFamilyPtr;
	IconFamilyHandle = IconFamilyPtrPtr;
{  Icon Variants }
{ These can be used as an element of an 'icns' icon family }
{ or as a parameter to GetIconRefVariant }
const
	kTileIconVariant = FourCharCode('tile');
	kRolloverIconVariant = FourCharCode('over');
	kDropIconVariant = FourCharCode('drop');
	kOpenIconVariant = FourCharCode('open');
	kOpenDropIconVariant = FourCharCode('odrp');


{$endc} {TARGET_OS_MAC}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
