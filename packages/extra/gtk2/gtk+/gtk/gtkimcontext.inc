// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkIMContext = ^TGtkIMContext;
   TGtkIMContext = record
        parent_instance : TGObject;
     end;

{ Signals  }
{ Virtual functions  }
{ Padding for future expansion  }
   PGtkIMContextClass = ^TGtkIMContextClass;
   TGtkIMContextClass = record
        parent_class : TGtkObjectClass;
        preedit_start : procedure (context:PGtkIMContext); cdecl;
        preedit_end : procedure (context:PGtkIMContext); cdecl;
        preedit_changed : procedure (context:PGtkIMContext); cdecl;
        commit : procedure (context:PGtkIMContext; str:Pgchar); cdecl;
        retrieve_surrounding : function (context:PGtkIMContext):gboolean; cdecl;
        delete_surrounding : function (context:PGtkIMContext; offset:gint; n_chars:gint):gboolean; cdecl;
        set_client_window : procedure (context:PGtkIMContext; window:PGdkWindow); cdecl;
        get_preedit_string : procedure (context:PGtkIMContext; str:PPgchar; var attrs:PPangoAttrList; cursor_pos:Pgint); cdecl;
        filter_keypress : function (context:PGtkIMContext; event:PGdkEventKey):gboolean; cdecl;
        focus_in : procedure (context:PGtkIMContext); cdecl;
        focus_out : procedure (context:PGtkIMContext); cdecl;
        reset : procedure (context:PGtkIMContext); cdecl;
        set_cursor_location : procedure (context:PGtkIMContext; area:PGdkRectangle); cdecl;
        set_use_preedit : procedure (context:PGtkIMContext; use_preedit:gboolean); cdecl;
        set_surrounding : procedure (context:PGtkIMContext; text:Pgchar; len:gint; cursor_index:gint); cdecl;
        get_surrounding : function (context:PGtkIMContext; text:PPgchar; cursor_index:Pgint):gboolean; cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
        _gtk_reserved5 : procedure ; cdecl;
        _gtk_reserved6 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_IM_CONTEXT : GType;
function GTK_IM_CONTEXT(obj: pointer) : PGtkIMContext;
function GTK_IM_CONTEXT_CLASS(klass: pointer) : PGtkIMContextClass;
function GTK_IS_IM_CONTEXT(obj: pointer) : boolean;
function GTK_IS_IM_CONTEXT_CLASS(klass: pointer) : boolean;
function GTK_IM_CONTEXT_GET_CLASS(obj: pointer) : PGtkIMContextClass;


function gtk_im_context_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_im_context_set_client_window(context:PGtkIMContext; window:PGdkWindow); cdecl; external gtklib;
procedure gtk_im_context_get_preedit_string(context:PGtkIMContext; str:PPgchar; var attrs:PPangoAttrList; cursor_pos:Pgint); cdecl; external gtklib;
function gtk_im_context_filter_keypress(context:PGtkIMContext; event:PGdkEventKey):gboolean; cdecl; external gtklib;
procedure gtk_im_context_focus_in(context:PGtkIMContext); cdecl; external gtklib;
procedure gtk_im_context_focus_out(context:PGtkIMContext); cdecl; external gtklib;
procedure gtk_im_context_reset(context:PGtkIMContext); cdecl; external gtklib;
procedure gtk_im_context_set_cursor_location(context:PGtkIMContext; area:PGdkRectangle); cdecl; external gtklib;
procedure gtk_im_context_set_use_preedit(context:PGtkIMContext; use_preedit:gboolean); cdecl; external gtklib;
procedure gtk_im_context_set_surrounding(context:PGtkIMContext; text:Pgchar; len:gint; cursor_index:gint); cdecl; external gtklib;
function gtk_im_context_get_surrounding(context:PGtkIMContext; text:PPgchar; cursor_index:Pgint):gboolean; cdecl; external gtklib;
function gtk_im_context_delete_surrounding(context:PGtkIMContext; offset:gint; n_chars:gint):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_IM_CONTEXT : GType;
begin
   GTK_TYPE_IM_CONTEXT:=gtk_im_context_get_type;
end;

function GTK_IM_CONTEXT(obj: pointer) : PGtkIMContext;
begin
   GTK_IM_CONTEXT:=PGtkIMContext(GTK_CHECK_CAST(obj,GTK_TYPE_IM_CONTEXT));
end;

function GTK_IM_CONTEXT_CLASS(klass: pointer) : PGtkIMContextClass;
begin
   GTK_IM_CONTEXT_CLASS:=PGtkIMContextClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_IM_CONTEXT));
end;

function GTK_IS_IM_CONTEXT(obj: pointer) : boolean;
begin
   GTK_IS_IM_CONTEXT:=GTK_CHECK_TYPE(obj,GTK_TYPE_IM_CONTEXT);
end;

function GTK_IS_IM_CONTEXT_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_IM_CONTEXT_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_IM_CONTEXT);
end;

function GTK_IM_CONTEXT_GET_CLASS(obj: pointer) : PGtkIMContextClass;
begin
   GTK_IM_CONTEXT_GET_CLASS:=PGtkIMContextClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_IM_CONTEXT));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

