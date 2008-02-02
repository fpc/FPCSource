{$IfDef read_interface}

function gnome_i18n_get_language:Pchar;cdecl;external libgnomedll name 'gnome_i18n_get_language';
function gnome_i18n_get_language_list(category_name:Pgchar):PGList;cdecl;external libgnomedll name 'gnome_i18n_get_language_list';
procedure gnome_i18n_set_preferred_language(val:Pchar);cdecl;external libgnomedll name 'gnome_i18n_set_preferred_language';
function gnome_i18n_get_preferred_language:Pchar;cdecl;external libgnomedll name 'gnome_i18n_get_preferred_language';
procedure gnome_i18n_init;cdecl;external libgnomedll name 'gnome_i18n_init';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
