{ Parsed from CFOpenDirectory.framework CFODQuery.h }

{$if  not (defined(__OPENDIRECTORY_CFODQUERY__))}
{$ifdef TYPES}
const
  __OPENDIRECTORY_CFODQUERY__ = 1;

type
  ODQueryCallback = procedure (query: ODQueryRef; results: CFArrayRef; error: CFErrorRef; context: pointer); cdecl;
{$endif}

{$ifdef FUNCTIONS}
function ODQueryGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODQueryCreateWithNode(allocator: CFAllocatorRef; node: ODNodeRef; recordTypeOrList: CFTypeRef; attribute: ODAttributeType; matchType: ODMatchType; queryValueOrList: CFTypeRef; returnAttributeOrList: CFTypeRef; maxResults: CFIndex; error: CFErrorRefPtr): ODQueryRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODQueryCreateWithNodeType(allocator: CFAllocatorRef; nodeType: ODNodeType; recordTypeOrList: CFTypeRef; attribute: ODAttributeType; matchType: ODMatchType; queryValueOrList: CFTypeRef; returnAttributeOrList: CFTypeRef; maxResults: CFIndex; error: CFErrorRefPtr): ODQueryRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODQueryCopyResults(query: ODQueryRef; allowPartialResults: cbool; error: CFErrorRefPtr): CFArrayRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
procedure ODQuerySynchronize(query: ODQueryRef); cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
procedure ODQuerySetCallback(query: ODQueryRef; callback: ODQueryCallback; userInfo: pointer); cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
procedure ODQueryScheduleWithRunLoop(query: ODQueryRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
procedure ODQueryUnscheduleFromRunLoop(query: ODQueryRef; runLoop: CFRunLoopRef; runLoopMode: CFStringRef); cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
procedure ODQuerySetDispatchQueue(query: ODQueryRef; queue: dispatch_queue_t); cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
{$endif}

{$endif}
