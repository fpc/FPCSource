// included by glib2.pas

{$IFDEF read_forward_definitions}
   TGBoxedCopyFunc = function (boxed:gpointer):gpointer; cdecl;
   TGBoxedFreeFunc = procedure (boxed:gpointer); cdecl;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$ifdef read_interface_rest}

(*** some macros ***)
function G_TYPE_IS_BOXED(_type : GType) : gboolean;
function G_VALUE_HOLDS_BOXED(value : PGValue) : gboolean;

(*** GLib boxed types ***)

function G_TYPE_CLOSURE : GType;
function G_TYPE_VALUE : GType;
function G_TYPE_VALUE_ARRAY : GType;
function G_TYPE_GSTRING : GType;


(*** prototypes ***)

function  g_boxed_copy(boxed_type : GType;
                       src_boxed  : gconstpointer):gpointer; cdecl; external gobjectlib;
procedure g_boxed_free(boxed_type : GType;
                       boxed      : gpointer); cdecl; external gobjectlib;
procedure g_value_set_boxed(value : PGValue;
                          v_boxed : gconstpointer); cdecl; external gobjectlib;
procedure g_value_set_static_boxed(value : PGValue;
                                 v_boxed : gconstpointer); cdecl; external gobjectlib;
function g_value_get_boxed( value : PGValue):gpointer; cdecl; external gobjectlib;
function g_value_dup_boxed( value : PGValue):gpointer; cdecl; external gobjectlib;
function g_boxed_type_register_static(name       : Pgchar;
                                      boxed_copy : TGBoxedCopyFunc;
                                      boxed_free : TGBoxedFreeFunc):GType; cdecl; external gobjectlib;

procedure g_value_set_boxed_take_ownership(value:PGValue;
                                           v_boxed:gconstpointer); cdecl; external gobjectlib;

function g_closure_get_type     : GType; cdecl; external gobjectlib;
function g_value_get_type       : GType; cdecl; external gobjectlib;
function g_value_array_get_type : GType; cdecl; external gobjectlib;
function g_gstring_get_type     : GType; cdecl; external gobjectlib;

{$endif read_interface_rest}
