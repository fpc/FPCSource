// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // find PGtkRcContext
   PGtkRcContext = pointer;

{ --- structures ---  }
{ of type GtkSettingsValue   }
   PGtkSettings = ^TGtkSettings;
   TGtkSettings = record
        parent_instance : TGObject;
        queued_settings : PGData;
        property_values : PGValue;
        rc_context : PGtkRcContext;
        screen : PGdkScreen;
     end;

   PGtkSettingsClass = ^TGtkSettingsClass;
   TGtkSettingsClass = record
        parent_class : TGObjectClass;
     end;

{ origin should be something like "filename:linenumber" for rc files,
     or e.g. "XProperty" for other sources
    }
{ valid types are LONG, DOUBLE and STRING corresponding to the token parsed,
     or a GSTRING holding an unparsed statement
    }
   PGtkSettingsValue = ^TGtkSettingsValue;
   TGtkSettingsValue = record
        origin : Pgchar;
        value : TGValue;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_SETTINGS : GType;
function GTK_SETTINGS(obj: pointer) : PGtkSettings;
function GTK_SETTINGS_CLASS(klass: pointer) : PGtkSettingsClass;
function GTK_IS_SETTINGS(obj: pointer) : boolean;
function GTK_IS_SETTINGS_CLASS(klass: pointer) : boolean;
function GTK_SETTINGS_GET_CLASS(obj: pointer) : PGtkSettingsClass;


{ --- functions ---  }

function gtk_settings_get_type:GType; cdecl; external gtklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gtk_settings_get_default:PGtkSettings; cdecl; external gtklib;
{$endif}

function gtk_settings_get_for_screen(screen:PGdkScreen):PGtkSettings; cdecl; external gtklib;
procedure gtk_settings_install_property(pspec:PGParamSpec); cdecl; external gtklib;
procedure gtk_settings_install_property_parser(pspec:PGParamSpec; parser:TGtkRcPropertyParser); cdecl; external gtklib;
{ --- precoded parsing functions ---  }
function gtk_rc_property_parse_color(pspec:PGParamSpec; gstring:PGString; property_value:PGValue):gboolean; cdecl; external gtklib;
function gtk_rc_property_parse_enum(pspec:PGParamSpec; gstring:PGString; property_value:PGValue):gboolean; cdecl; external gtklib;
function gtk_rc_property_parse_flags(pspec:PGParamSpec; gstring:PGString; property_value:PGValue):gboolean; cdecl; external gtklib;
function gtk_rc_property_parse_requisition(pspec:PGParamSpec; gstring:PGString; property_value:PGValue):gboolean; cdecl; external gtklib;
function gtk_rc_property_parse_border(pspec:PGParamSpec; gstring:PGString; property_value:PGValue):gboolean; cdecl; external gtklib;
{< private > }
procedure gtk_settings_set_property_value(settings:PGtkSettings; name:Pgchar; svalue:PGtkSettingsValue); cdecl; external gtklib;
procedure gtk_settings_set_string_property(settings:PGtkSettings; name:Pgchar; v_string:Pgchar; origin:Pgchar); cdecl; external gtklib;
procedure gtk_settings_set_long_property(settings:PGtkSettings; name:Pgchar; v_long:glong; origin:Pgchar); cdecl; external gtklib;
procedure gtk_settings_set_double_property(settings:PGtkSettings; name:Pgchar; v_double:gdouble; origin:Pgchar); cdecl; external gtklib;
{ implementation details  }
procedure _gtk_settings_handle_event(event:PGdkEventSetting); cdecl; external gtklib;
function _gtk_rc_property_parser_from_type(_type:GType):TGtkRcPropertyParser; cdecl; external gtklib;
function _gtk_settings_parse_convert(parser:TGtkRcPropertyParser; src_value:PGValue; pspec:PGParamSpec; dest_value:PGValue):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_SETTINGS : GType;
begin
   GTK_TYPE_SETTINGS:=gtk_settings_get_type;
end;

function GTK_SETTINGS(obj: pointer) : PGtkSettings;
begin
   GTK_SETTINGS:=PGtkSettings(GTK_CHECK_CAST(obj,GTK_TYPE_SETTINGS));
end;

function GTK_SETTINGS_CLASS(klass: pointer) : PGtkSettingsClass;
begin
   GTK_SETTINGS_CLASS:=PGtkSettingsClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_SETTINGS));
end;

function GTK_IS_SETTINGS(obj: pointer) : boolean;
begin
   GTK_IS_SETTINGS:=GTK_CHECK_TYPE(obj,GTK_TYPE_SETTINGS);
end;

function GTK_IS_SETTINGS_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_SETTINGS_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_SETTINGS);
end;

function GTK_SETTINGS_GET_CLASS(obj: pointer) : PGtkSettingsClass;
begin
   GTK_SETTINGS_GET_CLASS:=PGtkSettingsClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_SETTINGS));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

