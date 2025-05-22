unit wasm.storage.api;

interface

uses wasm.storage.shared;

function __storage_get_item(aStorageKind : Longint; aKey : PAnsiChar; aKeyLen : Integer; aResult : PPansiChar; aResultLen : PLongint) : longint; external storageExportName name storageFN_GetItem;

function __storage_key(aStorageKind : Longint; aKey : integer; aResult : PPAnsichar; aResultLen : PLongint) : longint; external storageExportName name storageFN_Key;

function __storage_length(aStorageKind : Longint; aResult : PLongint) : longint;external storageExportName name storageFN_length;

function __storage_set_item(aStorageKind : Longint; aKey : PAnsiChar; aKeyLen : Integer; aValue : PAnsiChar; aValueLen : Integer) : longint; external storageExportName name storageFN_SetItem;

function __storage_remove_item(aStorageKind : Longint; aKey : PAnsiChar; aKeyLen : Integer) : longint; external storageExportName name storageFN_RemoveItem;

function __storage_clear(aStorageKind : Longint) : longint; external storageExportName name storageFN_Clear;

implementation

end.
