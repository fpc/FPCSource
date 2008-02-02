{$IfDef read_interface}

function gnome_mime_get_value(mime_type:Pchar; key:Pchar):Pchar;cdecl;external libgnomedll name 'gnome_mime_get_value';
function gnome_mime_get_keys(mime_type:Pchar):PGList;cdecl;external libgnomedll name 'gnome_mime_get_keys';
function gnome_mime_program(mime_type:Pchar):Pchar;cdecl;external libgnomedll name 'gnome_mime_program';
function gnome_mime_description(mime_type:Pchar):Pchar;cdecl;external libgnomedll name 'gnome_mime_description';
function gnome_mime_test(mime_type:Pchar):Pchar;cdecl;external libgnomedll name 'gnome_mime_test';
function gnome_mime_composetyped(mime_type:Pchar):Pchar;cdecl;external libgnomedll name 'gnome_mime_composetyped';
function gnome_mime_copiousoutput(mime_type:Pchar; key:Pchar):gboolean;cdecl;external libgnomedll name 'gnome_mime_copiousoutput';
function gnome_mime_needsterminal(mime_type:Pchar; key:Pchar):gboolean;cdecl;external libgnomedll name 'gnome_mime_needsterminal';

{$EndIf read_interface}

{$Ifdef read_implementation}

{$Endif read_implementation}
