{$IfDef read_interface}

function gconf_schema_new:PGConfSchema;cdecl;external gconfdll name 'gconf_schema_new';
procedure gconf_schema_free(sc:PGConfSchema);cdecl;external gconfdll name 'gconf_schema_free';
function gconf_schema_copy(sc:PGConfSchema):PGConfSchema;cdecl;external gconfdll name 'gconf_schema_copy';
procedure gconf_schema_set_type(sc:PGConfSchema; thetype:TGConfValueType);cdecl;external gconfdll name 'gconf_schema_set_type';
procedure gconf_schema_set_list_type(sc:PGConfSchema; thetype:TGConfValueType);cdecl;external gconfdll name 'gconf_schema_set_list_type';
procedure gconf_schema_set_car_type(sc:PGConfSchema; thetype:TGConfValueType);cdecl;external gconfdll name 'gconf_schema_set_car_type';
procedure gconf_schema_set_cdr_type(sc:PGConfSchema; thetype:TGConfValueType);cdecl;external gconfdll name 'gconf_schema_set_cdr_type';
procedure gconf_schema_set_locale(sc:PGConfSchema; locale:Pgchar);cdecl;external gconfdll name 'gconf_schema_set_locale';
procedure gconf_schema_set_short_desc(sc:PGConfSchema; desc:Pgchar);cdecl;external gconfdll name 'gconf_schema_set_short_desc';
procedure gconf_schema_set_long_desc(sc:PGConfSchema; desc:Pgchar);cdecl;external gconfdll name 'gconf_schema_set_long_desc';
procedure gconf_schema_set_owner(sc:PGConfSchema; theowner:Pgchar);cdecl;external gconfdll name 'gconf_schema_set_owner';
procedure gconf_schema_set_default_value(sc:PGConfSchema; val:PGConfValue);cdecl;external gconfdll name 'gconf_schema_set_default_value';
procedure gconf_schema_set_default_value_nocopy(sc:PGConfSchema; val:PGConfValue);cdecl;external gconfdll name 'gconf_schema_set_default_value_nocopy';

function gconf_schema_get_type(x : PGConfSchema) : TGConfValueType;
function gconf_schema_get_list_type(x : PGConfSchema) : TGConfValueType;
function gconf_schema_get_car_type(x : PGConfSchema) : TGConfValueType;
function gconf_schema_get_cdr_type(x : PGConfSchema) : TGConfValueType;

function gconf_schema_get_locale(x : PGConfSchema) : Pgchar;
function gconf_schema_get_short_desc(x : PGConfSchema) : Pgchar;
function gconf_schema_get_long_desc(x : PGConfSchema) : Pgchar;
function gconf_schema_get_owner(x : PGConfSchema) : Pgchar;
function gconf_schema_get_default_value(x : PGConfSchema) : PGConfValue;

{$EndIf read_interface}

{$Ifdef read_implementation}

function gconf_schema_get_type(x : PGConfSchema) : TGConfValueType;
begin
   gconf_schema_get_type := x^.thetype;
end;

function gconf_schema_get_list_type(x : PGConfSchema) : TGConfValueType;
begin
   gconf_schema_get_list_type:=x^.list_type;
end;

function gconf_schema_get_car_type(x : PGConfSchema) : TGConfValueType;
begin
   gconf_schema_get_car_type:=x^.car_type;
end;

function gconf_schema_get_cdr_type(x : PGConfSchema) : TGConfValueType;
begin
   gconf_schema_get_cdr_type:=x^.cdr_type;
end;

function gconf_schema_get_locale(x : PGConfSchema) : Pgchar;
begin
   gconf_schema_get_locale:=x^.locale;
end;

function gconf_schema_get_short_desc(x : PGConfSchema) : Pgchar;
begin
   gconf_schema_get_short_desc:=x^.short_desc;
end;

function gconf_schema_get_long_desc(x : PGConfSchema) : Pgchar;
begin
   gconf_schema_get_long_desc:=x^.long_desc;
end;

function gconf_schema_get_owner(x : PGConfSchema) : Pgchar;
begin
   gconf_schema_get_owner:=x^.theowner;
end;

function gconf_schema_get_default_value(x : PGConfSchema) : PGConfValue;
begin
   gconf_schema_get_default_value:=x^.default_value;
end;

{$Endif read_implementation}
