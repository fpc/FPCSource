{$IfDef read_interface}
function GCONF_VALUE_TYPE_VALID(x : longint) : gboolean;

function gconf_value_get_string(x : PGConfValue) : Pgchar;
function gconf_value_get_int(x : PGConfValue) : gint;
function gconf_value_get_float(x : PGConfValue) : gfloat;
function gconf_value_get_list_type(x : PGConfValue) : TGConfValueType;
function gconf_value_get_list(x : PGConfValue) : PGSList;
function gconf_value_get_car(x : PGConfValue) : PGConfValue;
function gconf_value_get_cdr(x : PGConfValue) : PGConfValue;
function gconf_value_get_bool(x : PGConfValue) : gboolean;
function gconf_value_get_schema(x : PGConfValue) : PGConfSchema;

function gconf_value_new(_type:TGConfValueType):PGConfValue;cdecl;external gconfdll name 'gconf_value_new';
function gconf_value_copy(src:PGConfValue):PGConfValue;cdecl;external gconfdll name 'gconf_value_copy';
procedure gconf_value_free(value:PGConfValue);cdecl;external gconfdll name 'gconf_value_free';
procedure gconf_value_set_int(value:PGConfValue; the_int:gint);cdecl;external gconfdll name 'gconf_value_set_int';
procedure gconf_value_set_string(value:PGConfValue; the_str:Pgchar);cdecl;external gconfdll name 'gconf_value_set_string';
procedure gconf_value_set_float(value:PGConfValue; the_float:gdouble);cdecl;external gconfdll name 'gconf_value_set_float';
procedure gconf_value_set_bool(value:PGConfValue; the_bool:gboolean);cdecl;external gconfdll name 'gconf_value_set_bool';
procedure gconf_value_set_schema(value:PGConfValue; sc:PGConfSchema);cdecl;external gconfdll name 'gconf_value_set_schema';
procedure gconf_value_set_schema_nocopy(value:PGConfValue; sc:PGConfSchema);cdecl;external gconfdll name 'gconf_value_set_schema_nocopy';
procedure gconf_value_set_car(value:PGConfValue; car:PGConfValue);cdecl;external gconfdll name 'gconf_value_set_car';
procedure gconf_value_set_car_nocopy(value:PGConfValue; car:PGConfValue);cdecl;external gconfdll name 'gconf_value_set_car_nocopy';
procedure gconf_value_set_cdr(value:PGConfValue; cdr:PGConfValue);cdecl;external gconfdll name 'gconf_value_set_cdr';
procedure gconf_value_set_cdr_nocopy(value:PGConfValue; cdr:PGConfValue);cdecl;external gconfdll name 'gconf_value_set_cdr_nocopy';
procedure gconf_value_set_list_type(value:PGConfValue; thetype:TGConfValueType);cdecl;external gconfdll name 'gconf_value_set_list_type';
procedure gconf_value_set_list_nocopy(value:PGConfValue; list:PGSList);cdecl;external gconfdll name 'gconf_value_set_list_nocopy';
procedure gconf_value_set_list(value:PGConfValue; list:PGSList);cdecl;external gconfdll name 'gconf_value_set_list';
function gconf_value_to_string(value:PGConfValue):Pgchar;cdecl;external gconfdll name 'gconf_value_to_string';

type
   PGConfMetaInfo =  ^TGConfMetaInfo;
   TGConfMetaInfo = record
        schema : Pgchar;
        mod_user : Pgchar;
        mod_time : TGTime;
     end;

function gconf_meta_info_get_schema(x : PGConfMetaInfo) : Pgchar;
function gconf_meta_info_get_mod_user(x : PGConfMetaInfo) : Pgchar;
function gconf_meta_info_mod_time(x : PGConfMetaInfo) : TGTime;

function gconf_meta_info_new:PGConfMetaInfo;cdecl;external gconfdll name 'gconf_meta_info_new';
procedure gconf_meta_info_free(gcmi:PGConfMetaInfo);cdecl;external gconfdll name 'gconf_meta_info_free';
procedure gconf_meta_info_set_schema(gcmi:PGConfMetaInfo; schema_name:Pgchar);cdecl;external gconfdll name 'gconf_meta_info_set_schema';
procedure gconf_meta_info_set_mod_user(gcmi:PGConfMetaInfo; mod_user:Pgchar);cdecl;external gconfdll name 'gconf_meta_info_set_mod_user';
procedure gconf_meta_info_set_mod_time(gcmi:PGConfMetaInfo; mod_time:TGTime);cdecl;external gconfdll name 'gconf_meta_info_set_mod_time';

type
   PGConfEntry = ^TGConfEntry;
   TGConfEntry = record
        key : Pgchar;
        value : PGConfValue;
        schema_name : Pgchar;
        flag0 : word;
     end;

const
   bm__GConfEntry_is_default = $1;
   bp__GConfEntry_is_default = 0;
   bm__GConfEntry_is_writable = $2;
   bp__GConfEntry_is_writable = 1;
function is_default(var a : TGConfEntry) : gboolean;
procedure set_is_default(var a : TGConfEntry; __is_default : gboolean);
function is_writable(var a : TGConfEntry) : gboolean;
procedure set_is_writable(var a : TGConfEntry; __is_writable : gboolean);

function gconf_entry_get_key(x : PGConfEntry) : Pgchar;
function gconf_entry_get_value(x : PGConfEntry) : PGConfValue;
function gconf_entry_get_schema_name(x : PGConfEntry) : Pgchar;
function gconf_entry_get_is_default(x : PGConfEntry) : gboolean;
function gconf_entry_get_is_writable(x : PGConfEntry) : gboolean;

function gconf_entry_new(key:Pgchar; val:PGConfValue):PGConfEntry;cdecl;external gconfdll name 'gconf_entry_new';
function gconf_entry_new_nocopy(key:Pgchar; val:PGConfValue):PGConfEntry;cdecl;external gconfdll name 'gconf_entry_new_nocopy';
procedure gconf_entry_free(entry:PGConfEntry);cdecl;external gconfdll name 'gconf_entry_free';
function gconf_entry_steal_value(entry:PGConfEntry):PGConfValue;cdecl;external gconfdll name 'gconf_entry_steal_value';
procedure gconf_entry_set_value(entry:PGConfEntry; val:PGConfValue);cdecl;external gconfdll name 'gconf_entry_set_value';
procedure gconf_entry_set_value_nocopy(entry:PGConfEntry; val:PGConfValue);cdecl;external gconfdll name 'gconf_entry_set_value_nocopy';

procedure gconf_entry_set_schema_name(entry:PGConfEntry; name:Pgchar);cdecl;external gconfdll name 'gconf_entry_set_schema_name';
procedure gconf_entry_set_is_default(entry:PGConfEntry; is_default:gboolean);cdecl;external gconfdll name 'gconf_entry_set_is_default';
procedure gconf_entry_set_is_writable(entry:PGConfEntry; is_writable:gboolean);cdecl;external gconfdll name 'gconf_entry_set_is_writable';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GCONF_VALUE_TYPE_VALID(x : longint) : gboolean;
begin
   GCONF_VALUE_TYPE_VALID:=(x > Longint(GCONF_VALUE_INVALID)) and (x <= Longint(GCONF_VALUE_PAIR));
end;

function gconf_value_get_string(x : PGConfValue) : Pgchar;
begin
   gconf_value_get_string:=x^.d.string_data;
end;

function gconf_value_get_int(x : PGConfValue) : gint;
begin
   gconf_value_get_int:=x^.d.int_data;
end;

function gconf_value_get_float(x : PGConfValue) : gfloat;
begin
   gconf_value_get_float:=x^.d.float_data;
end;

function gconf_value_get_list_type(x : PGConfValue) : TGConfValueType;
begin
   gconf_value_get_list_type:=x^.d.list_data.listtype;
end;

function gconf_value_get_list(x : PGConfValue) : PGSList;
begin
   gconf_value_get_list:=x^.d.list_data.list;
end;

function gconf_value_get_car(x : PGConfValue) : PGConfValue;
begin
   gconf_value_get_car:=x^.d.pair_data.car;
end;

function gconf_value_get_cdr(x : PGConfValue) : PGConfValue;
begin
   gconf_value_get_cdr:=x^.d.pair_data.cdr;
end;

function gconf_value_get_bool(x : PGConfValue) : gboolean;
begin
   gconf_value_get_bool:=x^.d.bool_data;
end;

function gconf_value_get_schema(x : PGConfValue) : PGConfSchema;
begin
   gconf_value_get_schema:=x^.d.schema_data;
end;

function gconf_meta_info_get_schema(x : PGConfMetaInfo) : Pgchar;
begin
   gconf_meta_info_get_schema:=x^.schema;
end;

function gconf_meta_info_get_mod_user(x : PGConfMetaInfo) : Pgchar;
begin
   gconf_meta_info_get_mod_user:=x^.mod_user;
end;

function gconf_meta_info_mod_time(x : PGConfMetaInfo) : TGTime;
begin
   gconf_meta_info_mod_time:=x^.mod_time;
end;

function is_default(var a : TGConfEntry) : gBoolean;
begin
   is_default:=gBoolean((a.flag0 and bm__GConfEntry_is_default) shr bp__GConfEntry_is_default);
end;

procedure set_is_default(var a : TGConfEntry; __is_default : gBoolean);
begin
   a.flag0:=a.flag0 or ((gint(__is_default) shl bp__GConfEntry_is_default) and bm__GConfEntry_is_default);
end;

function is_writable(var a : TGConfEntry) : gBoolean;
begin
   is_writable:=gBoolean((a.flag0 and bm__GConfEntry_is_writable) shr bp__GConfEntry_is_writable);
end;

procedure set_is_writable(var a : TGConfEntry; __is_writable : gBoolean);
begin
   a.flag0:=a.flag0 or ((gint(__is_writable) shl bp__GConfEntry_is_writable) and bm__GConfEntry_is_writable);
end;

function gconf_entry_get_key(x : PGConfEntry) : Pgchar;
begin
   gconf_entry_get_key:=x^.key;
end;

function gconf_entry_get_value(x : PGConfEntry) : PGConfValue;
begin
   gconf_entry_get_value:=x^.value;
end;

function gconf_entry_get_schema_name(x : PGConfEntry) : Pgchar;
begin
   gconf_entry_get_schema_name:=x^.schema_name;
end;

function gconf_entry_get_is_default(x : PGConfEntry) : gBoolean;
begin
   gconf_entry_get_is_default:=is_default(X^);
end;

function gconf_entry_get_is_writable(x : PGConfEntry) : gBoolean;
begin
   gconf_entry_get_is_writable:=is_writable(X^);
end;

{$Endif read_implementation}
