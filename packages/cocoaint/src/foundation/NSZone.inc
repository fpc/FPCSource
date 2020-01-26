{ Parsed from Foundation.framework NSZone.h }

{$ifdef TYPES}
//type
//  NSZone = OpaqueType;
//	NSZonePtr = ^NSZone;
{$endif}

{$ifdef FUNCTIONS}
function NSDefaultMallocZone: NSZonePtr; cdecl; external;
function NSCreateZone(startSize: NSUInteger; granularity: NSUInteger; canFree: ObjCBOOL): NSZonePtr; cdecl; external;
procedure NSRecycleZone(zone: NSZonePtr); cdecl; external;
procedure NSSetZoneName(zone: NSZonePtr; name: NSString); cdecl; external;
function NSZoneName(zone: NSZonePtr): NSString; cdecl; external;
function NSZoneFromPointer(ptr: pointer): NSZonePtr; cdecl; external;
function NSZoneMalloc(zone: NSZonePtr; size: NSUInteger): pointer; cdecl; external;
function NSZoneCalloc(zone: NSZonePtr; numElems: NSUInteger; byteSize: NSUInteger): pointer; cdecl; external;
function NSZoneRealloc(zone: NSZonePtr; ptr: pointer; size: NSUInteger): pointer; cdecl; external;
procedure NSZoneFree(zone: NSZonePtr; ptr: pointer); cdecl; external;
{$endif}

{$if (defined(TARGET_OS_MAC) and not (defined(TARGET_OS_EMBEDDED) or defined(TARGET_OS_IPHONE)))}
{$ifdef TYPES}
const
  NSScannedOption = 1 shl 0;
  NSCollectorDisabledOption = 1 shl 1;
{$endif}

{$ifdef FUNCTIONS}
function NSAllocateCollectable(size: NSUInteger; options: NSUInteger): pointer; cdecl; external;
function NSReallocateCollectable(ptr: pointer; size: NSUInteger; options: NSUInteger): pointer; cdecl; external;
{$endif}

{$endif}

{$ifdef FUNCTIONS}
function NSPageSize: NSUInteger; cdecl; external;
function NSLogPageSize: NSUInteger; cdecl; external;
function NSRoundUpToMultipleOfPageSize(bytes: NSUInteger): NSUInteger; cdecl; external;
function NSRoundDownToMultipleOfPageSize(bytes: NSUInteger): NSUInteger; cdecl; external;
function NSAllocateMemoryPages(bytes: NSUInteger): pointer; cdecl; external;
procedure NSDeallocateMemoryPages(ptr: pointer; bytes: NSUInteger); cdecl; external;
procedure NSCopyMemoryPages(source: pointer; dest: pointer; bytes: NSUInteger); cdecl; external;
function NSRealMemoryAvailable: NSUInteger; cdecl; external; deprecated 'in 10_0, 10_8, 2_0, 6_0';
{$endif}

