{$IfDef read_interface}
type
   TGnomeMetadataError = (GNOME_METADATA_OK := 0,GNOME_METADATA_IO_ERROR,
     GNOME_METADATA_NOT_FOUND);

function gnome_metadata_set(afile:PAnsiChar; name:PAnsiChar; size:longint; data:PAnsiChar):longint;cdecl;external libgnomedll name 'gnome_metadata_set';
function gnome_metadata_remove(afile:PAnsiChar; name:PAnsiChar):longint;cdecl;external libgnomedll name 'gnome_metadata_remove';
function gnome_metadata_list(afile:PAnsiChar):PPAnsiChar;cdecl;external libgnomedll name 'gnome_metadata_list';
function gnome_metadata_get(afile:PAnsiChar; name:PAnsiChar; size:Plongint; buffer:PPAnsiChar):longint;cdecl;external libgnomedll name 'gnome_metadata_get';
function gnome_metadata_get_fast(afile:PAnsiChar; name:PAnsiChar; size:Plongint; buffer:PPAnsiChar):longint;cdecl;external libgnomedll name 'gnome_metadata_get_fast';
function gnome_metadata_rename(isfrom:PAnsiChar; isto:PAnsiChar):longint;cdecl;external libgnomedll name 'gnome_metadata_rename';
function gnome_metadata_copy(isfrom:PAnsiChar; isto:PAnsiChar):longint;cdecl;external libgnomedll name 'gnome_metadata_copy';
function gnome_metadata_delete(afile:PAnsiChar):longint;cdecl;external libgnomedll name 'gnome_metadata_delete';
procedure gnome_metadata_regex_add(regex:PAnsiChar; key:PAnsiChar; size:longint; data:PAnsiChar);cdecl;external libgnomedll name 'gnome_metadata_regex_add';
procedure gnome_metadata_regex_remove(regex:PAnsiChar; key:PAnsiChar);cdecl;external libgnomedll name 'gnome_metadata_regex_remove';
procedure gnome_metadata_type_add(thetype:PAnsiChar; key:PAnsiChar; size:longint; data:PAnsiChar);cdecl;external libgnomedll name 'gnome_metadata_type_add';
procedure gnome_metadata_type_remove(thetype:PAnsiChar; key:PAnsiChar);cdecl;external libgnomedll name 'gnome_metadata_type_remove';
procedure gnome_metadata_lock;cdecl;external libgnomedll name 'gnome_metadata_lock';
procedure gnome_metadata_unlock;cdecl;external libgnomedll name 'gnome_metadata_unlock';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
