{ Parsed from Foundation.framework NSZone.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.2 - Tue Sep 8 15:31:00 ICT 2009 }


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
function NSCreateZone(startSize: culong; granularity: culong; canFree: Boolean): NSZone; cdecl; external name 'NSCreateZone';
procedure NSRecycleZone(var zone: NSZone); cdecl; external name 'NSRecycleZone';
procedure NSSetZoneName(var zone: NSZone; var name: NSString); cdecl; external name 'NSSetZoneName';
function NSZoneName(var zone: NSZone): NSString; cdecl; external name 'NSZoneName';
function NSZoneFromPointer(var ptr: Pointer): NSZone; cdecl; external name 'NSZoneFromPointer';
procedure NSZoneMalloc(var zone: NSZone; size: culong); cdecl; external name 'NSZoneMalloc';
procedure NSZoneCalloc(var zone: NSZone; numElems: culong; byteSize: culong); cdecl; external name 'NSZoneCalloc';
procedure NSZoneRealloc(var zone: NSZone; var ptr: Pointer; size: culong); cdecl; external name 'NSZoneRealloc';
procedure NSZoneFree(var zone: NSZone; var ptr: Pointer); cdecl; external name 'NSZoneFree';
function NSPageSize: culong; cdecl; external name 'NSPageSize';
function NSLogPageSize: culong; cdecl; external name 'NSLogPageSize';
function NSRoundUpToMultipleOfPageSize(bytes: culong): culong; cdecl; external name 'NSRoundUpToMultipleOfPageSize';
function NSRoundDownToMultipleOfPageSize(bytes: culong): culong; cdecl; external name 'NSRoundDownToMultipleOfPageSize';
procedure NSAllocateMemoryPages(bytes: culong); cdecl; external name 'NSAllocateMemoryPages';
procedure NSDeallocateMemoryPages(var ptr: Pointer; bytes: culong); cdecl; external name 'NSDeallocateMemoryPages';
procedure NSCopyMemoryPages(var source: Pointer; var dest: Pointer; bytes: culong); cdecl; external name 'NSCopyMemoryPages';
function NSRealMemoryAvailable: culong; cdecl; external name 'NSRealMemoryAvailable';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSZONE_PAS_T}
{$define NSZONE_PAS_T}

{$endif}
{$endif}
