{$IfDef read_interface}
function _gnome_config_get_string_with_default(path:Pchar; def:Pgboolean; priv:gint):pchar;cdecl;external libgnomedll name '_gnome_config_get_string_with_default';
function _gnome_config_get_translated_string_with_default(path:Pchar; def:Pgboolean; priv:gint):pchar;cdecl;external libgnomedll name '_gnome_config_get_translated_string_with_default';
function _gnome_config_get_int_with_default(path:Pchar; def:Pgboolean; priv:gint):gint;cdecl;external libgnomedll name '_gnome_config_get_int_with_default';
function _gnome_config_get_float_with_default(path:Pchar; def:Pgboolean; priv:gint):gdouble;cdecl;external libgnomedll name '_gnome_config_get_float_with_default';
function _gnome_config_get_bool_with_default(path:Pchar; def:Pgboolean; priv:gint):gboolean;cdecl;external libgnomedll name '_gnome_config_get_bool_with_default';
Procedure _gnome_config_get_vector_with_default(path:Pchar; argcp:Pgint; argvp:PPPchar; def:Pgboolean; priv:gint);cdecl;external libgnomedll name '_gnome_config_get_vector_with_default';

function gnome_config_get_string_with_default(path:Pchar; def:Pgboolean):pchar;
function gnome_config_get_translated_string_with_default(path:Pchar; def:Pgboolean):pchar;
function gnome_config_get_int_with_default(path:Pchar; def:Pgboolean):gint;
function gnome_config_get_float_with_default(path:Pchar; def:Pgboolean):gdouble;
function gnome_config_get_bool_with_default(path:Pchar; def:Pgboolean):gboolean;
Procedure gnome_config_get_vector_with_default(path:Pchar; argcp:Pgint; argvp:PPPchar; def:Pgboolean);

function gnome_config_private_get_string_with_default(path:Pchar; def:Pgboolean):pchar;
function gnome_config_private_get_translated_string_with_default(path:Pchar; def:Pgboolean):pchar;
function gnome_config_private_get_int_with_default(path:Pchar; def:Pgboolean):gint;
function gnome_config_private_get_float_with_default(path:Pchar; def:Pgboolean):gdouble;
function gnome_config_private_get_bool_with_default(path:Pchar; def:Pgboolean):gboolean;
procedure gnome_config_private_get_vector_with_default(path:Pchar; argcp:Pgint; argvp:PPPchar; def:Pgboolean);

function gnome_config_get_string(path:Pchar):pchar;
function gnome_config_get_translated_string(path:Pchar):pchar;
function gnome_config_get_int(path:Pchar):gint;
function gnome_config_get_float(path:Pchar):gdouble;
function gnome_config_get_bool(path:Pchar):gboolean;
procedure gnome_config_get_vector(path:Pchar; argcp:Pgint; argvp:PPPchar);

function gnome_config_private_get_string(path:Pchar):pchar;
function gnome_config_private_get_translated_string(path:Pchar):pchar;
function gnome_config_private_get_int(path:Pchar):gint;
function gnome_config_private_get_float(path:Pchar):gdouble;
function gnome_config_private_get_bool(path:Pchar):gboolean;
procedure gnome_config_private_get_vector(path:Pchar; argcp:Pgint; argvp:PPPchar);

procedure _gnome_config_set_string(path:Pchar; value:Pchar; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_string';
procedure _gnome_config_set_translated_string(path:Pchar; value:Pchar; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_translated_string';
procedure _gnome_config_set_int(path:Pchar; value:longint; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_int';
procedure _gnome_config_set_float(path:Pchar; value:gdouble; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_float';
procedure _gnome_config_set_bool(path:Pchar; value:gboolean; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_bool';
procedure _gnome_config_set_vector(path:Pchar; argc:longint; argv:PPchar; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_vector';

procedure gnome_config_set_string(path,new_value : Pchar);
procedure gnome_config_set_translated_string(path, new_value : PChar);
procedure gnome_config_set_int(path : PChar; new_value : longint);
procedure gnome_config_set_float(path : PChar; new_value : longint);
procedure gnome_config_set_bool(path : PChar; new_value : gboolean);
procedure gnome_config_set_vector(path:Pchar; argc:longint; argv:PPchar);

procedure gnome_config_private_set_string(path,new_value : Pchar);
procedure gnome_config_private_set_translated_string(path, new_value : PChar);
procedure gnome_config_private_set_int(path : Pchar; new_value : longint);
procedure gnome_config_private_set_float(path : PChar; new_value : longint);
procedure gnome_config_private_set_bool(path : PChar; new_value : gboolean);
procedure gnome_config_private_set_vector(path:Pchar; argc:longint; argv:PPchar);

function _gnome_config_has_section(path:Pchar; priv:gint):gboolean;cdecl;external libgnomedll name '_gnome_config_has_section';

function gnome_config_has_section(path : Pchar) : gboolean;

function gnome_config_private_has_section(path : pchar) : gboolean;

function _gnome_config_init_iterator(path:Pchar; priv:gint):pointer;cdecl;external libgnomedll name '_gnome_config_init_iterator';
function _gnome_config_init_iterator_sections(path:Pchar; priv:gint):pointer;cdecl;external libgnomedll name '_gnome_config_init_iterator_sections';
function gnome_config_iterator_next(iterator_handle:pointer; key:PPchar; value:PPchar):pointer;cdecl;external libgnomedll name 'gnome_config_iterator_next';

function gnome_config_init_iterator(path : pchar) : pointer;
function gnome_config_init_iterator_sections(path : Pchar) : pointer;

function gnome_config_private_init_iterator(path : Pchar) : pointer;
function gnome_config_private_init_iterator_sections(path : Pchar) : pointer;

procedure gnome_config_drop_all;cdecl;external libgnomedll name 'gnome_config_drop_all';
procedure gnome_config_sync;cdecl;external libgnomedll name 'gnome_config_sync';
procedure _gnome_config_sync_file(path:Pchar; priv:longint);cdecl;external libgnomedll name '_gnome_config_sync_file';

procedure gnome_config_sync_file(path : Pchar);

procedure gnome_config_private_sync_file(path : Pchar);

procedure _gnome_config_drop_file(path:Pchar; priv:gint);cdecl;external libgnomedll name '_gnome_config_drop_file';

procedure gnome_config_drop_file(path : Pchar);

procedure gnome_config_private_drop_file(path : Pchar);

procedure _gnome_config_clean_file(path:Pchar; priv:gint);cdecl;external libgnomedll name '_gnome_config_clean_file';

Procedure gnome_config_clean_file(path:Pchar);

Procedure gnome_config_private_clean_file(path:Pchar);

procedure _gnome_config_clean_section(path:Pchar; priv:gint);cdecl;external libgnomedll name '_gnome_config_clean_section';

procedure gnome_config_clean_section(path:Pchar);

procedure gnome_config_private_clean_section(path:Pchar);

procedure _gnome_config_clean_key(path:Pchar; priv:gint);cdecl;external libgnomedll name '_gnome_config_clean_key';

procedure gnome_config_clean_key(path:Pchar);

procedure gnome_config_private_clean_key(path:Pchar);

function gnome_config_get_real_path(path:Pchar):Pchar;

function gnome_config_private_get_real_path(path:Pchar):Pchar;

procedure gnome_config_push_prefix(path:Pchar);cdecl;external libgnomedll name 'gnome_config_push_prefix';
procedure gnome_config_pop_prefix;cdecl;external libgnomedll name 'gnome_config_pop_prefix';
procedure gnome_config_make_vector(_string:Pchar; argcp:Plongint; argvp:PPPchar);cdecl;external libgnomedll name 'gnome_config_make_vector';
function gnome_config_assemble_vector(argc:longint; argv:PPchar):pchar;cdecl;external libgnomedll name 'gnome_config_assemble_vector';

Type
  TGnomeConfigHandler = procedure (param : pointer); cdecl;

procedure gnome_config_set_set_handler(func : TGnomeConfigHandler; data:pointer);cdecl;external libgnomedll name 'gnome_config_set_set_handler';
procedure gnome_config_set_sync_handler(func:TGnomeConfigHandler; data:pointer);cdecl;external libgnomedll name 'gnome_config_set_sync_handler';

{$EndIf read_interface}

{$Ifdef read_implementation}

function gnome_config_get_string_with_default(path:Pchar; def:Pgboolean):pchar;
begin
   gnome_config_get_string_with_default:=_gnome_config_get_string_with_default(path,def,0);
end;

function gnome_config_get_translated_string_with_default(path:Pchar; def:Pgboolean):pchar;
begin
   gnome_config_get_translated_string_with_default:=_gnome_config_get_translated_string_with_default(path,def,0);
end;

function gnome_config_get_int_with_default(path:Pchar; def:Pgboolean):gint;
begin
   gnome_config_get_int_with_default:=_gnome_config_get_int_with_default(path,def,0);
end;

function gnome_config_get_float_with_default(path:Pchar; def:Pgboolean):gdouble;
begin
   gnome_config_get_float_with_default:=_gnome_config_get_float_with_default(path,def,0);
end;

function gnome_config_get_bool_with_default(path:Pchar; def:Pgboolean):gboolean;
begin
   gnome_config_get_bool_with_default:=_gnome_config_get_bool_with_default(path,def,0);
end;

procedure gnome_config_get_vector_with_default(path:Pchar; argcp:Pgint; argvp:PPPchar; def:Pgboolean);
begin
   _gnome_config_get_vector_with_default(path,argcp,argvp,def,0);
end;

function gnome_config_private_get_string_with_default(path:Pchar; def:Pgboolean):pchar;
begin
   gnome_config_private_get_string_with_default:=_gnome_config_get_string_with_default(path,def,1);
end;

function gnome_config_private_get_translated_string_with_default(path:Pchar; def:Pgboolean):pchar;
begin
   gnome_config_private_get_translated_string_with_default:=_gnome_config_get_translated_string_with_default(path,def,1);
end;

function gnome_config_private_get_int_with_default(path:Pchar; def:Pgboolean):gint;
begin
   gnome_config_private_get_int_with_default:=_gnome_config_get_int_with_default(path,def,1);
end;

function gnome_config_private_get_float_with_default(path:Pchar; def:Pgboolean):gdouble;
begin
   gnome_config_private_get_float_with_default:=_gnome_config_get_float_with_default(path,def,1);
end;

function gnome_config_private_get_bool_with_default(path:Pchar; def:Pgboolean):gboolean;
begin
   gnome_config_private_get_bool_with_default:=_gnome_config_get_bool_with_default(path,def,1);
end;

procedure gnome_config_private_get_vector_with_default(path:Pchar; argcp:Pgint; argvp:PPPchar; def:Pgboolean);
begin
   _gnome_config_get_vector_with_default(path,argcp,argvp,def,1);
end;

function gnome_config_get_string(path:Pchar):pchar;
begin
   gnome_config_get_string:=_gnome_config_get_string_with_default(path,nil,0);
end;

function gnome_config_get_translated_string(path:Pchar):pchar;
begin
   gnome_config_get_translated_string:=_gnome_config_get_translated_string_with_default(path,nil,0);
end;

function gnome_config_get_int(path:Pchar):gint;
begin
   gnome_config_get_int:=_gnome_config_get_int_with_default(path,nil,0);
end;

function gnome_config_get_float(path:Pchar):gdouble;
begin
   gnome_config_get_float:=_gnome_config_get_float_with_default(path,nil,0);
end;

function gnome_config_get_bool(path:Pchar):gboolean;
begin
   gnome_config_get_bool:=_gnome_config_get_bool_with_default(path,nil,0);
end;

procedure gnome_config_get_vector(path:Pchar; argcp:Pgint; argvp:PPPchar);
begin
   _gnome_config_get_vector_with_default(path,argcp,argvp,nil,0);
end;

function gnome_config_private_get_string(path:Pchar):pchar;
begin
   gnome_config_private_get_string:=_gnome_config_get_string_with_default(path,nil,1);
end;

function gnome_config_private_get_translated_string(path:Pchar):pchar;
begin
   gnome_config_private_get_translated_string:=_gnome_config_get_translated_string_with_default(path,nil,1);
end;

function gnome_config_private_get_int(path:Pchar):gint;
begin
   gnome_config_private_get_int:=_gnome_config_get_int_with_default(path,nil,1);
end;

function gnome_config_private_get_float(path:Pchar):gdouble;
begin
   gnome_config_private_get_float:=_gnome_config_get_float_with_default(path,nil,1);
end;

function gnome_config_private_get_bool(path:Pchar):gboolean;
begin
   gnome_config_private_get_bool:=_gnome_config_get_bool_with_default(path,nil,1);
end;

procedure gnome_config_private_get_vector(path:Pchar; argcp:Pgint; argvp:PPPchar);
begin
   _gnome_config_get_vector_with_default(path,argcp,argvp,nil,1);
end;

procedure gnome_config_set_string(path,new_value : Pchar);
begin
   _gnome_config_set_string(path,new_value,0);
end;

procedure gnome_config_set_translated_string(path, new_value : PChar);
begin
   _gnome_config_set_translated_string(path,new_value,0);
end;

procedure gnome_config_set_int(path : PChar; new_value : longint);
begin
   _gnome_config_set_int(path,new_value,0);
end;

procedure gnome_config_set_float(path : PChar; new_value : longint);
begin
   _gnome_config_set_float(path,new_value,0);
end;

procedure gnome_config_set_bool(path : PChar; new_value : gboolean);
begin
   _gnome_config_set_bool(path,new_value,0);
end;

procedure gnome_config_set_vector(path:Pchar; argc:longint; argv:PPchar);
begin
   _gnome_config_set_vector(path,argc,argv,0);
end;

procedure gnome_config_private_set_string(path,new_value : Pchar);
begin
   _gnome_config_set_string(path,new_value,1);
end;

procedure gnome_config_private_set_translated_string(path, new_value : PChar);
begin
   _gnome_config_set_translated_string(path,new_value,1);
end;

procedure gnome_config_private_set_int(path : PChar; new_value : longint);
begin
   _gnome_config_set_int(path,new_value,1);
end;

procedure gnome_config_private_set_float(path : PChar; new_value : longint);
begin
   _gnome_config_set_float(path,new_value,1);
end;

procedure gnome_config_private_set_bool(path : PChar; new_value : gboolean);
begin
   _gnome_config_set_bool(path,new_value,1);
end;

procedure gnome_config_private_set_vector(path:Pchar; argc:longint; argv:PPchar);
begin
   _gnome_config_set_vector(path,argc,argv,1);
end;

function gnome_config_has_section(path : Pchar) : gboolean;
begin
   gnome_config_has_section:=_gnome_config_has_section(path,0);
end;

function gnome_config_private_has_section(path : pchar) : gboolean;
begin
   gnome_config_private_has_section:=_gnome_config_has_section(path,1);
end;

function gnome_config_init_iterator(path : pchar) : pointer;
begin
   gnome_config_init_iterator:=_gnome_config_init_iterator(path,0);
end;

function gnome_config_init_iterator_sections(path : Pchar) : pointer;
begin
   gnome_config_init_iterator_sections:=_gnome_config_init_iterator_sections(path,0);
end;

function gnome_config_private_init_iterator(path : Pchar) : pointer;
begin
   gnome_config_private_init_iterator:=_gnome_config_init_iterator(path,1);
end;

function gnome_config_private_init_iterator_sections(path : Pchar) : pointer;
begin
   gnome_config_private_init_iterator_sections:=_gnome_config_init_iterator_sections(path,1);
end;

procedure gnome_config_sync_file(path : Pchar);
begin
   _gnome_config_sync_file(path,0);
end;

procedure gnome_config_private_sync_file(path : Pchar);
begin
   _gnome_config_sync_file(path,1);
end;

procedure gnome_config_drop_file(path : Pchar);
begin
   _gnome_config_drop_file(path,0);
end;

procedure gnome_config_private_drop_file(path : Pchar);
begin
   _gnome_config_drop_file(path,1);
end;

Procedure gnome_config_clean_file(path:Pchar);
begin
   _gnome_config_clean_file(path,0);
end;

Procedure gnome_config_private_clean_file(path:Pchar);
begin
   _gnome_config_clean_file(path,1);
end;

procedure gnome_config_clean_section(path:Pchar);
begin
   _gnome_config_clean_section(path,0);
end;

procedure gnome_config_private_clean_section(path:Pchar);
begin
   _gnome_config_clean_section(path,1);
end;

procedure gnome_config_clean_key(path:Pchar);
begin
   _gnome_config_clean_key(path,0);
end;

procedure gnome_config_private_clean_key(path:Pchar);
begin
   _gnome_config_clean_key(path,1);
end;

function gnome_config_get_real_path(path : pchar) : pchar;
begin
  gnome_config_get_real_path := g_concat_dir_and_file(gnome_user_dir, Path);
end;

function gnome_config_private_get_real_path(path : pchar) : pchar;
begin
  gnome_config_private_get_real_path := g_concat_dir_and_file(gnome_user_private_dir, Path);
end;

{$Endif read_implementation}
