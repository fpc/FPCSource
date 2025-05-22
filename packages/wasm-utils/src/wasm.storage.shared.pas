unit wasm.storage.shared;

{$mode ObjFPC}

interface

const
  STORAGE_LOCAL   = 0;
  STORAGE_SESSION = 1;

  ESTORAGE_SUCCESS     = 0;
  ESTORAGE_INVALIDKIND = -1;

  storageExportName    = 'storage';
  storageFN_GetItem    = 'storage_get_item';
  storageFN_Key        = 'storage_key';
  storageFN_length     = 'storage_length';
  storageFN_SetItem    = 'storage_set_item';
  storageFN_RemoveItem = 'storage_remove_item';
  storageFN_Clear      = 'storage_clear';


implementation

end.

