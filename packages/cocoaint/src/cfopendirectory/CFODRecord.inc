{ Parsed from CFOpenDirectory.framework CFODRecord.h }

{$if  not (defined(__OPENDIRECTORY_CFODRECORD__))}
{$ifdef FUNCTIONS}
function ODRecordGetTypeID: CFTypeID; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordSetNodeCredentials(record_: ODRecordRef; username: CFStringRef; password: CFStringRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordSetNodeCredentialsExtended(record_: ODRecordRef; recordType: ODRecordType; authType: ODAuthenticationType; authItems: CFArrayRef; outAuthItems: CFArrayRefPtr; outContext: ODContextRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordSetNodeCredentialsUsingKerberosCache(record_: ODRecordRef; cacheName: CFStringRef; error: CFErrorRefPtr): cbool; cdecl; external; deprecated 'in __MAC_10_6, __MAC_10_7, __IPHONE_NA, __IPHONE_NA';
function ODRecordCopyPasswordPolicy(allocator: CFAllocatorRef; record_: ODRecordRef; error: CFErrorRefPtr): CFDictionaryRef; cdecl; external; { available in __MAC_10_6, __MAC_10_9, __IPHONE_NA, __IPHONE_NA, "use ODRecordCopyEffectivePolicies" }
function ODRecordVerifyPassword(record_: ODRecordRef; password: CFStringRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordVerifyPasswordExtended(record_: ODRecordRef; authType: ODAuthenticationType; authItems: CFArrayRef; outAuthItems: CFArrayRefPtr; outContext: ODContextRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordChangePassword(record_: ODRecordRef; oldPassword: CFStringRef; newPassword: CFStringRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordGetRecordType(record_: ODRecordRef): CFStringRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordGetRecordName(record_: ODRecordRef): CFStringRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordCopyValues(record_: ODRecordRef; attribute: ODAttributeType; error: CFErrorRefPtr): CFArrayRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordSetValue(record_: ODRecordRef; attribute: ODAttributeType; valueOrValues: CFTypeRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordAddValue(record_: ODRecordRef; attribute: ODAttributeType; value: CFTypeRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordRemoveValue(record_: ODRecordRef; attribute: ODAttributeType; value: CFTypeRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordCopyDetails(record_: ODRecordRef; attributes: CFArrayRef; error: CFErrorRefPtr): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordSynchronize(record_: ODRecordRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordDelete(record_: ODRecordRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordAddMember(group: ODRecordRef; member: ODRecordRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordRemoveMember(group: ODRecordRef; member: ODRecordRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordContainsMember(group: ODRecordRef; member: ODRecordRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_6, __IPHONE_NA }
function ODRecordCopyPolicies(record_: ODRecordRef; error: CFErrorRefPtr): CFDictionaryRef; cdecl; external; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use ODRecordCopyAccountPolicies" }
function ODRecordCopyEffectivePolicies(record_: ODRecordRef; error: CFErrorRefPtr): CFDictionaryRef; cdecl; external; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use ODRecordAuthenticationAllowed and similar functions" }
function ODRecordCopySupportedPolicies(record_: ODRecordRef; error: CFErrorRefPtr): CFDictionaryRef; cdecl; external; deprecated 'in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA';
function ODRecordSetPolicies(record_: ODRecordRef; policies: CFDictionaryRef; error: CFErrorRefPtr): cbool; cdecl; external; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use ODRecordSetAccountPolicies" }
function ODRecordSetPolicy(record_: ODRecordRef; policy: ODPolicyType; value: CFTypeRef; error: CFErrorRefPtr): cbool; cdecl; external; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use ODRecordAddAccountPolicy" }
function ODRecordRemovePolicy(record_: ODRecordRef; policy: ODPolicyType; error: CFErrorRefPtr): cbool; cdecl; external; { available in __MAC_10_9, __MAC_10_10, __IPHONE_NA, __IPHONE_NA, "use ODRecordRemoveAccountPolicy" }
function ODRecordAddAccountPolicy(record_: ODRecordRef; policy: CFDictionaryRef; category: ODPolicyCategoryType; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
function ODRecordRemoveAccountPolicy(record_: ODRecordRef; policy: CFDictionaryRef; category: ODPolicyCategoryType; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
function ODRecordSetAccountPolicies(record_: ODRecordRef; policies: CFDictionaryRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
function ODRecordCopyAccountPolicies(record_: ODRecordRef; error: CFErrorRefPtr): CFDictionaryRef; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA } { CF_RETURNS_RETAINED }
function ODRecordAuthenticationAllowed(record_: ODRecordRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
function ODRecordPasswordChangeAllowed(record_: ODRecordRef; newPassword: CFStringRef; error: CFErrorRefPtr): cbool; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
function ODRecordWillPasswordExpire(record_: ODRecordRef; willExpireIn: cuint64): cbool; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
function ODRecordWillAuthenticationsExpire(record_: ODRecordRef; willExpireIn: cuint64): cbool; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
function ODRecordSecondsUntilPasswordExpires(record_: ODRecordRef): cint64; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
function ODRecordSecondsUntilAuthenticationsExpire(record_: ODRecordRef): cint64; cdecl; external; { available starting in __MAC_10_10, __IPHONE_NA }
{$endif}

{$endif}
