{ Parsed from CoreMedia.framework CMMemoryPool.h }

{$ifndef CMMEMORYPOOL_H}

{$ifdef TYPES}
type
  CMMemoryPoolRef = OpaqueType;
{$endif}

{$ifdef FUNCTIONS}
function CMMemoryPoolGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  kCMMemoryPoolOption_AgeOutPeriod: CFStringRef; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function CMMemoryPoolCreate(options: CFDictionaryRef): CMMemoryPoolRef; cdecl; external;
function CMMemoryPoolGetAllocator(pool: CMMemoryPoolRef): CFAllocatorRef; cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
procedure CMMemoryPoolFlush(pool: CMMemoryPoolRef); cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
procedure CMMemoryPoolInvalidate(pool: CMMemoryPoolRef); cdecl; external; { available starting in __MAC_10_8, __IPHONE_6_0 }
{$endif}

{$endif}
