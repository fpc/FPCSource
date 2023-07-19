{$IfDef read_interface}
function _gnome_config_get_string_with_default(path:PAnsiChar; def:Pgboolean; priv:gint):PAnsiChar;cdecl;external libgnomedll name '_gnome_config_get_string_with_default';
function _gnome_config_get_translated_string_with_default(path:PAnsiChar; def:Pgboolean; priv:gint):PAnsiChar;cdecl;external libgnomedll name '_gnome_config_get_translated_string_with_default';
function _gnome_config_get_int_with_default(path:PAnsiChar; def:Pgboolean; priv:gint):gint;cdecl;external libgnomedll name '_gnome_config_get_int_with_default';
function _gnome_config_get_float_with_default(path:PAnsiChar; def:Pgboolean; priv:gint):gdouble;cdecl;external libgnomedll name '_gnome_config_get_float_with_default';
function _gnome_config_get_bool_with_default(path:PAnsiChar; def:Pgboolean; priv:gint):gboolean;cdecl;external libgnomedll name '_gnome_config_get_bool_with_default';
Procedure _gnome_config_get_vector_with_default(path:PAnsiChar; argcp:Pgint; argvp:PPPAnsiChar; def:Pgboolean; priv:gint);cdecl;external libgnomedll name '_gnome_config_get_vector_with_default';

function gnome_config_get_string_with_default(path:PAnsiChar; def:Pgboolean):PAnsiChar;
function gnome_config_get_translated_string_with_default(path:PAnsiChar; def:Pgboolean):PAnsiChar;
function gnome_config_get_int_with_default(path:PAnsiChar; def:Pgboolean):gint;
function gnome_config_get_float_with_default(path:PAnsiChar; def:Pgboolean):gdouble;
function gnome_config_get_bool_with_default(path:PAnsiChar; def:Pgboolean):gboolean;
Procedure gnome_config_get_vector_with_default(path:PAnsiChar; argcp:Pgint; argvp:PPPAnsiChar; def:Pgboolean);

function gnome_config_private_get_string_with_default(path:PAnsiChar; def:Pgboolean):PAnsiChar;
function gnome_config_private_get_translated_string_with_default(path:PAnsiChar; def:Pgboolean):PAnsiChar;
function gnome_config_private_get_int_with_default(path:PAnsiChar; def:Pgboolean):gint;
function gnome_config_private_get_float_with_default(path:PAnsiChar; def:Pgboolean):gdouble;
function gnome_config_private_get_bool_with_default(path:PAnsiChar; def:Pgboolean):gboolean;
procedure gnome_config_private_get_vector_with_default(path:PAnsiChar; argcp:Pgint; argvp:PPPAnsiChar; def:Pgboolean);

function gnome_config_get_string(path:PAnsiChar):PAnsiChar;
function gnome_config_get_translated_string(path:PAnsiChar):PAnsiChar;
function gnome_config_get_int(path:PAnsiChar):gint;
function gnome_config_get_float(path:PAnsiChar):gdouble;
function gnome_config_get_bool(path:PAnsiChar):gboolean;
procedure gnome_config_get_vector(path:PAnsiChar; argcp:Pgint; argvp:PPPAnsiChar);

function gnome_config_private_get_string(path:PAnsiChar):PAnsiChar;
function gnome_config_private_get_translated_string(path:PAnsiChar):PAnsiChar;
function gnome_config_private_get_int(path:PAnsiChar):gint;
function gnome_config_private_get_float(path:PAnsiChar):gdouble;
function gnome_config_private_get_bool(path:PAnsiChar):gboolean;
procedure gnome_config_private_get_vector(path:PAnsiChar; argcp:Pgint; argvp:PPPAnsiChar);

procedure _gnome_config_set_string(path:PAnsiChar; value:PAnsiChar; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_string';
procedure _gnome_config_set_translated_string(path:PAnsiChar; value:PAnsiChar; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_translated_string';
procedure _gnome_config_set_int(path:PAnsiChar; value:longint; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_int';
procedure _gnome_config_set_float(path:PAnsiChar; value:gdouble; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_float';
procedure _gnome_config_set_bool(path:PAnsiChar; value:gboolean; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_bool';
procedure _gnome_config_set_vector(path:PAnsiChar; argc:longint; argv:PPAnsiChar; priv:gint);cdecl;external libgnomedll name '_gnome_config_set_vector';

procedure gnome_config_set_string(path,new_value : PAnsiChar);
procedure gnome_config_set_translated_string(path, new_value : PAnsiChar);
procedure gnome_config_set_int(path : PAnsiChar; new_value : longint);
procedure gnome_config_set_float(path : PAnsiChar; new_value : longint);
procedure gnome_config_set_bool(path : PAnsiChar; new_value : gboolean);
procedure gnome_config_set_vector(path:PAnsiChar; argc:longint; argv:PPAnsiChar);

procedure gnome_config_private_set_string(path,new_value : PAnsiChar);
procedure gnome_config_private_set_translated_string(path, new_value : PAnsiChar);
procedure gnome_config_private_set_int(path : PAnsiChar; new_value : longint);
procedure gnome_config_private_set_float(path : PAnsiChar; new_value : longint);
procedure gnome_config_private_set_bool(path : PAnsiChar; new_value : gboolean);
procedure gnome_config_private_set_vector(path:PAnsiChar; argc:longint; argv:PPAnsiChar);

function _gnome_config_has_section(path:PAnsiChar; priv:gint):gboolean;cdecl;external libgnomedll name '_gnome_config_has_section';

function gnome_config_has_section(path : PAnsiChar) : gboolean;

function gnome_config_private_has_section(path : PAnsiChar) : gboolean;

function _gnome_config_init_iterator(path:PAnsiChar; priv:gint):pointer;cdecl;external libgnomedll name '_gnome_config_init_iterator';
function _gnome_config_init_iterator_sections(path:PAnsiChar; priv:gint):pointer;cdecl;external libgnomedll name '_gnome_config_init_iterator_sections';
function gnome_config_iterator_next(iterator_handle:pointer; key:PPAnsiChar; value:PPAnsiChar):pointer;cdecl;external libgnomedll name 'gnome_config_iterator_next';

function gnome_config_init_iterator(path : PAnsiChar) : pointer;
function gnome_config_init_iterator_sections(path : PAnsiChar) : pointer;

function gnome_config_private_init_iterator(path : PAnsiChar) : pointer;
function gnome_config_private_init_iterator_sections(path : PAnsiChar) : pointer;

procedure gnome_config_drop_all;cdecl;external libgnomedll name 'gnome_config_drop_all';
procedure gnome_config_sync;cdecl;external libgnomedll name 'gnome_config_sync';
procedure _gnome_config_sync_file(path:PAnsiChar; priv:longint);cdecl;external libgnomedll name '_gnome_config_sync_file';

procedure gnome_config_sync_file(path : PAnsiChar);

procedure gnome_config_private_sync_file(path : PAnsiChar);

procedure _gnome_config_drop_file(path:PAnsiChar; priv:gint);cdecl;external libgnomedll name '_gnome_config_drop_file';

procedure gnome_config_drop_file(path : PAnsiChar);

procedure gnome_config_private_drop_file(path : PAnsiChar);

procedure _gnome_config_clean_file(path:PAnsiChar; priv:gint);cdecl;external libgnomedll name '_gnome_config_clean_file';

Procedure gnome_config_clean_file(path:PAnsiChar);

Procedure gnome_config_private_clean_file(path:PAnsiChar);

procedure _gnome_config_clean_section(path:PAnsiChar; priv:gint);cdecl;external libgnomedll name '_gnome_config_clean_section';

procedure gnome_config_clean_section(path:PAnsiChar);

procedure gnome_config_private_clean_section(path:PAnsiChar);

procedure _gnome_config_clean_key(path:PAnsiChar; priv:gint);cdecl;external libgnomedll name '_gnome_config_clean_key';

procedure gnome_config_clean_key(path:PAnsiChar);

procedure gnome_config_private_clean_key(path:PAnsiChar);

function gnome_config_get_real_path(path:PAnsiChar):PAnsiChar;

function gnome_config_private_get_real_path(path:PAnsiChar):PAnsiChar;

procedure gnome_config_push_prefix(path:PAnsiChar);cdecl;external libgnomedll name 'gnome_config_push_prefix';
procedure gnome_config_pop_prefix;cdecl;external libgnomedll name 'gnome_config_pop_prefix';
procedure gnome_config_make_vector(_string:PAnsiChar; argcp:Plongint; argvp:PPPAnsiChar);cdecl;external libgnomedll name 'gnome_config_make_vector';
function gnome_config_assemble_vector(argc:longint; argv:PPAnsiChar):PAnsiChar;cdecl;external libgnomedll name 'gnome_config_assemble_vector';

Type
  TGnomeConfigHandler = procedure (param : pointer); cdecl;

procedure gnome_config_set_set_handler(func : TGnomeConfigHandler; data:pointer);cdecl;external libgnomedll name 'gnome_config_set_set_handler';
procedure gnome_config_set_sync_handler(func:TGnomeConfigHandler; data:pointer);cdecl;external libgnomedll name 'gnome_config_set_sync_handler';

{$EndIf read_interface}

{$Ifdef read_implementation}

function gnome_config_get_string_with_default(path:PAnsiChar; def:Pgboolean):PAnsiChar;
begin
   gnome_config_get_string_with_default:=_gnome_config_get_string_with_default(path,def,0);
end;

function gnome_config_get_translated_string_with_default(path:PAnsiChar; def:Pgboolean):PAnsiChar;
begin
   gnome_config_get_translated_string_with_default:=_gnome_config_get_translated_string_with_default(path,def,0);
end;

function gnome_config_get_int_with_default(path:PAnsiChar; def:Pgboolean):gint;
begin
   gnome_config_get_int_with_default:=_gnome_config_get_int_with_default(path,def,0);
end;

function gnome_config_get_float_with_default(path:PAnsiChar; def:Pgboolean):gdouble;
begin
   gnome_config_get_float_with_default:=_gnome_config_get_float_with_default(path,def,0);
end;

function gnome_config_get_bool_with_default(path:PAnsiChar; def:Pgboolean):gboolean;
begin
   gnome_config_get_bool_with_default:=_gnome_config_get_bool_with_default(path,def,0);
end;

procedure gnome_config_get_vector_with_default(path:PAnsiChar; argcp:Pgint; argvp:PPPAnsiChar; def:Pgboolean);
begin
   _gnome_config_get_vector_with_default(path,argcp,argvp,def,0);
end;

function gnome_config_private_get_string_with_default(path:PAnsiChar; def:Pgboolean):PAnsiChar;
begin
   gnome_config_private_get_string_with_default:=_gnome_config_get_string_with_default(path,def,1);
end;

function gnome_config_private_get_translated_string_with_default(path:PAnsiChar; def:Pgboolean):PAnsiChar;
begin
   gnome_config_private_get_translated_string_with_default:=_gnome_config_get_translated_string_with_default(path,def,1);
end;

function gnome_config_private_get_int_with_default(path:PAnsiChar; def:Pgboolean):gint;
begin
   gnome_config_private_get_int_with_default:=_gnome_config_get_int_with_default(path,def,1);
end;

function gnome_config_private_get_float_with_default(path:PAnsiChar; def:Pgboolean):gdouble;
begin
   gnome_config_private_get_float_with_default:=_gnome_config_get_float_with_default(path,def,1);
end;

function gnome_config_private_get_bool_with_default(path:PAnsiChar; def:Pgboolean):gboolean;
begin
   gnome_config_private_get_bool_with_default:=_gnome_config_get_bool_with_default(path,def,1);
end;

procedure gnome_config_private_get_vector_with_default(path:PAnsiChar; argcp:Pgint; argvp:PPPAnsiChar; def:Pgboolean);
begin
   _gnome_config_get_vector_with_default(path,argcp,argvp,def,1);
end;

function gnome_config_get_string(path:PAnsiChar):PAnsiChar;
begin
   gnome_config_get_string:=_gnome_config_get_string_with_default(path,nil,0);
end;

function gnome_config_get_translated_string(path:PAnsiChar):PAnsiChar;
begin
   gnome_config_get_translated_string:=_gnome_config_get_translated_string_with_default(path,nil,0);
end;

function gnome_config_get_int(path:PAnsiChar):gint;
begin
   gnome_config_get_int:=_gnome_config_get_int_with_default(path,nil,0);
end;

function gnome_config_get_float(path:PAnsiChar):gdouble;
begin
   gnome_config_get_float:=_gnome_config_get_float_with_default(path,nil,0);
end;

function gnome_config_get_bool(path:PAnsiChar):gboolean;
begin
   gnome_config_get_bool:=_gnome_config_get_bool_with_default(path,nil,0);
end;

procedure gnome_config_get_vector(path:PAnsiChar; argcp:Pgint; argvp:PPPAnsiChar);
begin
   _gnome_config_get_vector_with_default(path,argcp,argvp,nil,0);
end;

function gnome_config_private_get_string(path:PAnsiChar):PAnsiChar;
begin
   gnome_config_private_get_string:=_gnome_config_get_string_with_default(path,nil,1);
end;

function gnome_config_private_get_translated_string(path:PAnsiChar):PAnsiChar;
begin
   gnome_config_private_get_translated_string:=_gnome_config_get_translated_string_with_default(path,nil,1);
end;

function gnome_config_private_get_int(path:PAnsiChar):gint;
begin
   gnome_config_private_get_int:=_gnome_config_get_int_with_default(path,nil,1);
end;

function gnome_config_private_get_float(path:PAnsiChar):gdouble;
begin
   gnome_config_private_get_float:=_gnome_config_get_float_with_default(path,nil,1);
end;

function gnome_config_private_get_bool(path:PAnsiChar):gboolean;
begin
   gnome_config_private_get_bool:=_gnome_config_get_bool_with_default(path,nil,1);
end;

procedure gnome_config_private_get_vector(path:PAnsiChar; argcp:Pgint; argvp:PPPAnsiChar);
begin
   _gnome_config_get_vector_with_default(path,argcp,argvp,nil,1);
end;

procedure gnome_config_set_string(path,new_value : PAnsiChar);
begin
   _gnome_config_set_string(path,new_value,0);
end;

procedure gnome_config_set_translated_string(path, new_value : PAnsiChar);
begin
   _gnome_config_set_translated_string(path,new_value,0);
end;

procedure gnome_config_set_int(path : PAnsiChar; new_value : longint);
begin
   _gnome_config_set_int(path,new_value,0);
end;

procedure gnome_config_set_float(path : PAnsiChar; new_value : longint);
begin
   _gnome_config_set_float(path,new_value,0);
end;

procedure gnome_config_set_bool(path : PAnsiChar; new_value : gboolean);
begin
   _gnome_config_set_bool(path,new_value,0);
end;

procedure gnome_config_set_vector(path:PAnsiChar; argc:longint; argv:PPAnsiChar);
begin
   _gnome_config_set_vector(path,argc,argv,0);
end;

procedure gnome_config_private_set_string(path,new_value : PAnsiChar);
begin
   _gnome_config_set_string(path,new_value,1);
end;

procedure gnome_config_private_set_translated_string(path, new_value : PAnsiChar);
begin
   _gnome_config_set_translated_string(path,new_value,1);
end;

procedure gnome_config_private_set_int(path : PAnsiChar; new_value : longint);
begin
   _gnome_config_set_int(path,new_value,1);
end;

procedure gnome_config_private_set_float(path : PAnsiChar; new_value : longint);
begin
   _gnome_config_set_float(path,new_value,1);
end;

procedure gnome_config_private_set_bool(path : PAnsiChar; new_value : gboolean);
begin
   _gnome_config_set_bool(path,new_value,1);
end;

procedure gnome_config_private_set_vector(path:PAnsiChar; argc:longint; argv:PPAnsiChar);
begin
   _gnome_config_set_vector(path,argc,argv,1);
end;

function gnome_config_has_section(path : PAnsiChar) : gboolean;
begin
   gnome_config_has_section:=_gnome_config_has_section(path,0);
end;

function gnome_config_private_has_section(path : PAnsiChar) : gboolean;
begin
   gnome_config_private_has_section:=_gnome_config_has_section(path,1);
end;

function gnome_config_init_iterator(path : PAnsiChar) : pointer;
begin
   gnome_config_init_iterator:=_gnome_config_init_iterator(path,0);
end;

function gnome_config_init_iterator_sections(path : PAnsiChar) : pointer;
begin
   gnome_config_init_iterator_sections:=_gnome_config_init_iterator_sections(path,0);
end;

function gnome_config_private_init_iterator(path : PAnsiChar) : pointer;
begin
   gnome_config_private_init_iterator:=_gnome_config_init_iterator(path,1);
end;

function gnome_config_private_init_iterator_sections(path : PAnsiChar) : pointer;
begin
   gnome_config_private_init_iterator_sections:=_gnome_config_init_iterator_sections(path,1);
end;

procedure gnome_config_sync_file(path : PAnsiChar);
begin
   _gnome_config_sync_file(path,0);
end;

procedure gnome_config_private_sync_file(path : PAnsiChar);
begin
   _gnome_config_sync_file(path,1);
end;

procedure gnome_config_drop_file(path : PAnsiChar);
begin
   _gnome_config_drop_file(path,0);
end;

procedure gnome_config_private_drop_file(path : PAnsiChar);
begin
   _gnome_config_drop_file(path,1);
end;

Procedure gnome_config_clean_file(path:PAnsiChar);
begin
   _gnome_config_clean_file(path,0);
end;

Procedure gnome_config_private_clean_file(path:PAnsiChar);
begin
   _gnome_config_clean_file(path,1);
end;

procedure gnome_config_clean_section(path:PAnsiChar);
begin
   _gnome_config_clean_section(path,0);
end;

procedure gnome_config_private_clean_section(path:PAnsiChar);
begin
   _gnome_config_clean_section(path,1);
end;

procedure gnome_config_clean_key(path:PAnsiChar);
begin
   _gnome_config_clean_key(path,0);
end;

procedure gnome_config_private_clean_key(path:PAnsiChar);
begin
   _gnome_config_clean_key(path,1);
end;

function gnome_config_get_real_path(path : PAnsiChar) : PAnsiChar;
begin
  gnome_config_get_real_path := g_concat_dir_and_file(gnome_user_dir, Path);
end;

function gnome_config_private_get_real_path(path : PAnsiChar) : PAnsiChar;
begin
  gnome_config_private_get_real_path := g_concat_dir_and_file(gnome_user_private_dir, Path);
end;

{$Endif read_implementation}
