{$IfDef read_interface}
type
   TGnomeMetadataError = (GNOME_METADATA_OK := 0,GNOME_METADATA_IO_ERROR,
     GNOME_METADATA_NOT_FOUND);

function gnome_metadata_set(afile:Pchar; name:Pchar; size:longint; data:Pchar):longint;cdecl;external libgnomedll name 'gnome_metadata_set';
function gnome_metadata_remove(afile:Pchar; name:Pchar):longint;cdecl;external libgnomedll name 'gnome_metadata_remove';
function gnome_metadata_list(afile:Pchar):PPchar;cdecl;external libgnomedll name 'gnome_metadata_list';
function gnome_metadata_get(afile:Pchar; name:Pchar; size:Plongint; buffer:PPchar):longint;cdecl;external libgnomedll name 'gnome_metadata_get';
function gnome_metadata_get_fast(afile:Pchar; name:Pchar; size:Plongint; buffer:PPchar):longint;cdecl;external libgnomedll name 'gnome_metadata_get_fast';
function gnome_metadata_rename(isfrom:Pchar; isto:Pchar):longint;cdecl;external libgnomedll name 'gnome_metadata_rename';
function gnome_metadata_copy(isfrom:Pchar; isto:Pchar):longint;cdecl;external libgnomedll name 'gnome_metadata_copy';
function gnome_metadata_delete(afile:Pchar):longint;cdecl;external libgnomedll name 'gnome_metadata_delete';
procedure gnome_metadata_regex_add(regex:Pchar; key:Pchar; size:longint; data:Pchar);cdecl;external libgnomedll name 'gnome_metadata_regex_add';
procedure gnome_metadata_regex_remove(regex:Pchar; key:Pchar);cdecl;external libgnomedll name 'gnome_metadata_regex_remove';
procedure gnome_metadata_type_add(thetype:Pchar; key:Pchar; size:longint; data:Pchar);cdecl;external libgnomedll name 'gnome_metadata_type_add';
procedure gnome_metadata_type_remove(thetype:Pchar; key:Pchar);cdecl;external libgnomedll name 'gnome_metadata_type_remove';
procedure gnome_metadata_lock;cdecl;external libgnomedll name 'gnome_metadata_lock';
procedure gnome_metadata_unlock;cdecl;external libgnomedll name 'gnome_metadata_unlock';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
