{$IfDef read_interface}

function gnome_mime_type(filename:Pgchar):Pchar;cdecl;external libgnomedll name 'gnome_mime_type';
function gnome_mime_type_or_default(filename:Pgchar; defaultv:Pgchar):Pchar;cdecl;external libgnomedll name 'gnome_mime_type_or_default';
function gnome_mime_type_of_file(existing_filename:Pchar):Pchar;cdecl;external libgnomedll name 'gnome_mime_type_of_file';
function gnome_mime_type_or_default_of_file(existing_filename:Pchar; defaultv:Pgchar):Pchar;cdecl;external libgnomedll name 'gnome_mime_type_or_default_of_file';
function gnome_mime_type_from_magic(filename:Pgchar):Pchar;cdecl;external libgnomedll name 'gnome_mime_type_from_magic';
function gnome_uri_list_extract_filenames(uri_list:Pgchar):PGList;cdecl;external libgnomedll name 'gnome_uri_list_extract_filenames';
function gnome_uri_list_extract_uris(uri_list:Pgchar):PGList;cdecl;external libgnomedll name 'gnome_uri_list_extract_uris';
procedure gnome_uri_list_free_strings(list:PGList);cdecl;external libgnomedll name 'gnome_uri_list_free_strings';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
