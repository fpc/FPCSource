{ Parsed from Foundation.framework NSZone.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Thu Sep 17 14:52:46 ICT 2009 }


{$ifdef TYPES}
{$ifndef NSZONE_PAS_T}
{$define NSZONE_PAS_T}

{ Types }
type
  _NSZone = Pointer;
  NSZone = _NSZone;

{ Constants }

const
  NSScannedOption = 1 shl 0;
  NSCollectorDisabledOption = 1 shl 1;

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSZONE_PAS_R}
{$define NSZONE_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSZONE_PAS_F}
{$define NSZONE_PAS_F}

{ Functions }
function NSDefaultMallocZone: NSZone; cdecl; external name 'NSDefaultMallocZone';
function NSCreateZone(startSize: NSUInteger; granularity: NSUInteger; canFree: Boolean): NSZone; cdecl; external name 'NSCreateZone';
procedure NSRecycleZone(zonePointer: NSZone); cdecl; external name 'NSRecycleZone';
procedure NSSetZoneName(zonePointer: NSZone; namePointer: NSString); cdecl; external name 'NSSetZoneName';
function NSZoneName(zonePointer: NSZone): NSString; cdecl; external name 'NSZoneName';
function NSZoneFromPointer(ptrPointer: Pointer): NSZone; cdecl; external name 'NSZoneFromPointer';
procedure NSZoneMalloc(zonePointer: NSZone; size: NSUInteger); cdecl; external name 'NSZoneMalloc';
procedure NSZoneCalloc(zonePointer: NSZone; numElems: NSUInteger; byteSize: NSUInteger); cdecl; external name 'NSZoneCalloc';
procedure NSZoneRealloc(zonePointer: NSZone; ptrPointer: Pointer; size: NSUInteger); cdecl; external name 'NSZoneRealloc';
procedure NSZoneFree(zonePointer: NSZone; ptrPointer: Pointer); cdecl; external name 'NSZoneFree';
function NSAllocateCollectable(size: NSUInteger; options: NSUInteger): Pointer; cdecl; external name 'NSAllocateCollectable';
function NSReallocateCollectable(ptrPointer: Pointer; size: NSUInteger; options: NSUInteger): Pointer; cdecl; external name 'NSReallocateCollectable';
function NSPageSize: NSUInteger; cdecl; external name 'NSPageSize';
function NSLogPageSize: NSUInteger; cdecl; external name 'NSLogPageSize';
function NSRoundUpToMultipleOfPageSize(bytes: NSUInteger): NSUInteger; cdecl; external name 'NSRoundUpToMultipleOfPageSize';
function NSRoundDownToMultipleOfPageSize(bytes: NSUInteger): NSUInteger; cdecl; external name 'NSRoundDownToMultipleOfPageSize';
procedure NSAllocateMemoryPages(bytes: NSUInteger); cdecl; external name 'NSAllocateMemoryPages';
procedure NSDeallocateMemoryPages(ptrPointer: Pointer; bytes: NSUInteger); cdecl; external name 'NSDeallocateMemoryPages';
procedure NSCopyMemoryPages(sourcePointer: Pointer; destPointer: Pointer; bytes: NSUInteger); cdecl; external name 'NSCopyMemoryPages';
function NSRealMemoryAvailable: NSUInteger; cdecl; external name 'NSRealMemoryAvailable';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSZONE_PAS_T}
{$define NSZONE_PAS_T}

{$endif}
{$endif}
