{ Parsed from CFOpenDirectory.framework CFODSession.h }

{$if  not (defined(__OPENDIRECTORY_CFODSESSION__))}
{$ifdef EXTERNAL_SYMBOLS}
var
  kODSessionDefault: ODSessionRef { available starting in __MAC_10_6, __IPHONE_NA }; cvar; external;
{$endif}

{$ifdef FUNCTIONS}
function ODSessionGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODSessionCreate(allocator: CFAllocatorRef; options: CFDictionaryRef; error: CFErrorRefPtr): ODSessionRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODSessionCopyNodeNames(allocator: CFAllocatorRef; session: ODSessionRef; error: CFErrorRefPtr): CFArrayRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
{$endif}

{$endif}
