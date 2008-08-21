// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGOptionContext = ^TGOptionContext;
   TGOptionContext = record 
   end;

   PGOptionGroup = ^TGOptionGroup;
   TGOptionGroup = record
   end;

   PGOptionFlags = ^TGOptionFlags;
   TGOptionFlags = (
     G_OPTION_FLAG_HIDDEN = 1 shl 0,
     G_OPTION_FLAG_IN_MAIN = 1 shl 1,
     G_OPTION_FLAG_REVERSE = 1 shl 2,
     G_OPTION_FLAG_NO_ARG = 1 shl 3,
     G_OPTION_FLAG_FILENAME = 1 shl 4,
     G_OPTION_FLAG_OPTIONAL_ARG = 1 shl 5,
     G_OPTION_FLAG_NOALIAS = 1 shl 6
   );

   PGOptionArg = ^TGOptionArg;
   TGOptionArg = (
     G_OPTION_ARG_NONE,
     G_OPTION_ARG_STRING,
     G_OPTION_ARG_INT,
     G_OPTION_ARG_CALLBACK,
     G_OPTION_ARG_FILENAME,
     G_OPTION_ARG_STRING_ARRAY,
     G_OPTION_ARG_FILENAME_ARRAY,
     G_OPTION_ARG_DOUBLE,
     G_OPTION_ARG_INT64
   );

   PGOptionError = ^TGOptionError;
   TGOptionError = (
     G_OPTION_ERROR_UNKNOWN_OPTION,
     G_OPTION_ERROR_BAD_VALUE,
     G_OPTION_ERROR_FAILED
   );

   PGOptionEntry = ^TGOptionEntry;
   TGOptionEntry = record
        long_name : Pgchar;
        short_name : gchar;
        flags : gint;
        arg : TGOptionArg;
        arg_data : gpointer;
        description : Pgchar;
        arg_description : Pgchar;
     end;

   TGOptionArgFunc = function (option_name:Pgchar; value:Pgchar; data:gpointer; error:PPGError):gboolean;cdecl;
   TGOptionParseFunc = function (context:PGOptionContext; group:PGOptionGroup; data:gpointer; error:PPGError):gboolean;cdecl;
   TGOptionErrorFunc = procedure (context:PGOptionContext; group:PGOptionGroup; data:gpointer; error:PPGError);cdecl;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   G_OPTION_REMAINING = '';   

function G_OPTION_ERROR : GType;

function g_option_error_quark:TGQuark;cdecl;external gliblib;

function g_option_context_new(parameter_string:Pgchar):PGOptionContext;cdecl;external gliblib;
procedure g_option_context_set_summary(context:PGOptionContext; summary:Pgchar);cdecl;external gliblib;
function g_option_context_get_summary(context:PGOptionContext):Pgchar;cdecl;external gliblib;
procedure g_option_context_set_description(context:PGOptionContext; description:Pgchar);cdecl;external gliblib;
function g_option_context_get_description(context:PGOptionContext):Pgchar;cdecl;external gliblib;
procedure g_option_context_free(context:PGOptionContext);cdecl;external gliblib;
procedure g_option_context_set_help_enabled(context:PGOptionContext; help_enabled:gboolean);cdecl;external gliblib;
function g_option_context_get_help_enabled(context:PGOptionContext):gboolean;cdecl;external gliblib;
procedure g_option_context_set_ignore_unknown_options(context:PGOptionContext; ignore_unknown:gboolean);cdecl;external gliblib;
function g_option_context_get_ignore_unknown_options(context:PGOptionContext):gboolean;cdecl;external gliblib;
procedure g_option_context_add_main_entries(context:PGOptionContext; entries:PGOptionEntry; translation_domain:Pgchar);cdecl;external gliblib;
function g_option_context_parse(context:PGOptionContext; argc:Pgint; argv:PPPgchar; error:PPGError):gboolean;cdecl;external gliblib;
procedure g_option_context_set_translate_func(context:PGOptionContext; func:TGTranslateFunc; data:gpointer; destroy_notify:TGDestroyNotify);cdecl;external gliblib;
procedure g_option_context_set_translation_domain(context:PGOptionContext; domain:Pgchar);cdecl;external gliblib;
procedure g_option_context_add_group(context:PGOptionContext; group:PGOptionGroup);cdecl;external gliblib;
procedure g_option_context_set_main_group(context:PGOptionContext; group:PGOptionGroup);cdecl;external gliblib;
function g_option_context_get_main_group(context:PGOptionContext):PGOptionGroup;cdecl;external gliblib;
function g_option_context_get_help(context:PGOptionContext; main_help:gboolean; group:PGOptionGroup):Pgchar;cdecl;external gliblib;
function g_option_group_new(name:Pgchar; description:Pgchar; help_description:Pgchar; user_data:gpointer; destroy:TGDestroyNotify):PGOptionGroup;cdecl;external gliblib;
procedure g_option_group_set_parse_hooks(group:PGOptionGroup; pre_parse_func:TGOptionParseFunc; post_parse_func:TGOptionParseFunc);cdecl;external gliblib;
procedure g_option_group_set_error_hook(group:PGOptionGroup; error_func:TGOptionErrorFunc);cdecl;external gliblib;
procedure g_option_group_free(group:PGOptionGroup);cdecl;external gliblib;
procedure g_option_group_add_entries(group:PGOptionGroup; entries:PGOptionEntry);cdecl;external gliblib;
procedure g_option_group_set_translate_func(group:PGOptionGroup; func:TGTranslateFunc; data:gpointer; destroy_notify:TGDestroyNotify);cdecl;external gliblib;
procedure g_option_group_set_translation_domain(group:PGOptionGroup; domain:Pgchar);cdecl;external gliblib;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function G_OPTION_ERROR : GType;
begin
  G_OPTION_ERROR:=g_option_error_quark;
end;

{$ENDIF read_implementation}
// included by glib2.pas

