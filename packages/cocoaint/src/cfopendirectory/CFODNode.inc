{ Parsed from CFOpenDirectory.framework CFODNode.h }

{$if  not (defined(__OPENDIRECTORY_CFODNODE__))}
{$ifdef TYPES}
const
  __OPENDIRECTORY_CFODNODE__ = 1;
{$endif}

{$ifdef FUNCTIONS}
function ODNodeGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeCreateWithNodeType(allocator: CFAllocatorRef; session: ODSessionRef; nodeType: ODNodeType; error: CFErrorRefPtr): ODNodeRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeCreateWithName(allocator: CFAllocatorRef; session: ODSessionRef; nodeName: CFStringRef; error: CFErrorRefPtr): ODNodeRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeCreateCopy(allocator: CFAllocatorRef; node: ODNodeRef; error: CFErrorRefPtr): ODNodeRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeCopySubnodeNames(node: ODNodeRef; error: CFErrorRefPtr): CFArrayRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeCopyUnreachableSubnodeNames(node: ODNodeRef; error: CFErrorRefPtr): CFArrayRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeGetName(node: ODNodeRef): CFStringRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeCopyDetails(node: ODNodeRef; keys: CFArrayRef; error: CFErrorRefPtr): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeCopySupportedRecordTypes(node: ODNodeRef; error: CFErrorRefPtr): CFArrayRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeCopySupportedAttributes(node: ODNodeRef; recordType: ODRecordType; error: CFErrorRefPtr): CFArrayRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeSetCredentials(node: ODNodeRef; recordType: ODRecordType; recordName: CFStringRef; password: CFStringRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeSetCredentialsExtended(node: ODNodeRef; recordType: ODRecordType; authType: ODAuthenticationType; authItems: CFArrayRef; outAuthItems: CFArrayRefPtr; outContext: ODContextRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeSetCredentialsUsingKerberosCache(node: ODNodeRef; cacheName: CFStringRef; error: CFErrorRefPtr): cbool; cdecl; external; deprecated 'in __MAC_10_6, __MAC_10_7, __IPHONE_NA, __IPHONE_NA';
function ODNodeCreateRecord(node: ODNodeRef; recordType: ODRecordType; recordName: CFStringRef; attributeDict: CFDictionaryRef; error: CFErrorRefPtr): ODRecordRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeCopyRecord(node: ODNodeRef; recordType: ODRecordType; recordName: CFStringRef; attributes: CFTypeRef; error: CFErrorRefPtr): ODRecordRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODNodeCustomCall(node: ODNodeRef; customCode: CFIndex; data: CFDataRef; error: CFErrorRefPtr): CFDataRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA } { CF_RETURNS_RETAINED }
function ODNodeCustomFunction(node: ODNodeRef; function_: CFStringRef; payload: CFTypeRef; error: CFErrorRefPtr): CFTypeRef; cdecl; external; { available starting in __MAC_10_9, __IPHONE_NA } { CF_RETURNS_RETAINED }
function ODNodeCopyPolicies(node: ODNodeRef; error: CFErrorRefPtr): CFDictionaryRef; cdecl; external; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use ODNodeCopyAccountPolicies" }
function ODNodeCopySupportedPolicies(node: ODNodeRef; error: CFErrorRefPtr): CFDictionaryRef; cdecl; external; deprecated 'in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA';
function ODNodeSetPolicies(node: ODNodeRef; policies: CFDictionaryRef; error: CFErrorRefPtr): cbool; cdecl; external; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use ODNodeSetAccountPolicies" }
function ODNodeSetPolicy(node: ODNodeRef; policyType: ODPolicyType; value: CFTypeRef; error: CFErrorRefPtr): cbool; cdecl; external; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use ODNodeAddAccountPolicy" }
function ODNodeRemovePolicy(node: ODNodeRef; policyType: ODPolicyType; error: CFErrorRefPtr): cbool; cdecl; external; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use ODNodeRemoveAccountPolicy" }
function ODNodeAddAccountPolicy(node: ODNodeRef; policy: CFDictionaryRef; category: ODPolicyCategoryType; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
function ODNodeRemoveAccountPolicy(node: ODNodeRef; policy: CFDictionaryRef; category: ODPolicyCategoryType; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
function ODNodeSetAccountPolicies(node: ODNodeRef; policies: CFDictionaryRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
function ODNodeCopyAccountPolicies(node: ODNodeRef; error: CFErrorRefPtr): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA } { CF_RETURNS_RETAINED }
function ODNodePasswordContentCheck(node: ODNodeRef; password: CFStringRef; recordName: CFStringRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
{$endif}

{$endif}
