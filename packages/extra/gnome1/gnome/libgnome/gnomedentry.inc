{$IfDef read_interface}
type
   PGnomeDesktopEntry = ^TGnomeDesktopEntry;
   TGnomeDesktopEntry = record
        name : Pchar;
        comment : Pchar;
        exec_length : longint;
        exec : PPchar;
        tryexec : Pchar;
        icon : Pchar;
        docpath : Pchar;
        terminal : longint;
        thetype : Pchar;
        location : Pchar;
        geometry : Pchar;
        flag0 : word;
     end;

const
   bm_GnomeDesktopEntry_multiple_args = $1;
   bp_GnomeDesktopEntry_multiple_args = 0;
   bm_GnomeDesktopEntry_is_kde = $2;
   bp_GnomeDesktopEntry_is_kde = 1;

function multiple_args(var a : TGnomeDesktopEntry) : Boolean;
procedure set_multiple_args(var a : TGnomeDesktopEntry; __multiple_args : Boolean);
function is_kde(var a : TGnomeDesktopEntry) : Boolean;
procedure set_is_kde(var a : TGnomeDesktopEntry; __is_kde : Boolean);

function gnome_desktop_entry_load(afile:Pchar):PGnomeDesktopEntry;cdecl;external libgnomedll name 'gnome_desktop_entry_load';
function gnome_desktop_entry_load_flags(afile:Pchar; clean_from_memory_after_load:longint):PGnomeDesktopEntry;cdecl;external libgnomedll name 'gnome_desktop_entry_load_flags';
function gnome_desktop_entry_load_flags_conditional(afile:Pchar; clean_from_memory_after_load:longint; unconditional:longint):PGnomeDesktopEntry;cdecl;external libgnomedll name 'gnome_desktop_entry_load_flags_conditional';
function gnome_desktop_entry_load_unconditional(afile:Pchar):PGnomeDesktopEntry;cdecl;external libgnomedll name 'gnome_desktop_entry_load_unconditional';

procedure gnome_desktop_entry_save(dentry:PGnomeDesktopEntry);cdecl;external libgnomedll name 'gnome_desktop_entry_save';
procedure gnome_desktop_entry_free(item:PGnomeDesktopEntry);cdecl;external libgnomedll name 'gnome_desktop_entry_free';
procedure gnome_desktop_entry_destroy(item:PGnomeDesktopEntry);cdecl;external libgnomedll name 'gnome_desktop_entry_destroy';
procedure gnome_desktop_entry_launch(item:PGnomeDesktopEntry);cdecl;external libgnomedll name 'gnome_desktop_entry_launch';
procedure gnome_desktop_entry_launch_with_args(item:PGnomeDesktopEntry; the_argc:longint; the_argv:array of Pchar);cdecl;external libgnomedll name 'gnome_desktop_entry_launch_with_args';
function gnome_desktop_entry_copy(source:PGnomeDesktopEntry):PGnomeDesktopEntry;cdecl;external libgnomedll name 'gnome_desktop_entry_copy';

type
   PGnomeDesktopEntryI18N = ^TGnomeDesktopEntryI18N;
   TGnomeDesktopEntryI18N = record
        lang : Pchar;
        name : Pchar;
        comment : Pchar;
     end;

function gnome_desktop_entry_get_i18n_list(item:PGnomeDesktopEntry):PGList;cdecl;external libgnomedll name 'gnome_desktop_entry_get_i18n_list';
procedure gnome_desktop_entry_set_i18n_list(item:PGnomeDesktopEntry; list:PGList);cdecl;external libgnomedll name 'gnome_desktop_entry_set_i18n_list';
procedure gnome_desktop_entry_free_i18n_list(list:PGList);cdecl;external libgnomedll name 'gnome_desktop_entry_free_i18n_list';

{$EndIf read_interface}

{$Ifdef read_implementation}

function multiple_args(var a : TGnomeDesktopEntry) : Boolean;
begin
   multiple_args:=Boolean((a.flag0 and bm_GnomeDesktopEntry_multiple_args) shr bp_GnomeDesktopEntry_multiple_args);
end;

procedure set_multiple_args(var a : TGnomeDesktopEntry; __multiple_args : Boolean);
begin
   a.flag0:=a.flag0 or ((longint(__multiple_args) shl bp_GnomeDesktopEntry_multiple_args) and bm_GnomeDesktopEntry_multiple_args);
end;

function is_kde(var a : TGnomeDesktopEntry) : Boolean;
begin
   is_kde:=Boolean((a.flag0 and bm_GnomeDesktopEntry_is_kde) shr bp_GnomeDesktopEntry_is_kde);
end;

procedure set_is_kde(var a : TGnomeDesktopEntry; __is_kde : Boolean);
begin
   a.flag0:=a.flag0 or ((Longint(__is_kde) shl bp_GnomeDesktopEntry_is_kde) and bm_GnomeDesktopEntry_is_kde);
end;

{$Endif read_implementation}
