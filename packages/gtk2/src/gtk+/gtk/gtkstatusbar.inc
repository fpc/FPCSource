// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkStatusbar = ^TGtkStatusbar;
   TGtkStatusbar = record
        parent_widget : TGtkHBox;
        frame : PGtkWidget;
        _label : PGtkWidget;
        messages : PGSList;
        keys : PGSList;
        seq_context_id : guint;
        seq_message_id : guint;
        grip_window : PGdkWindow;
        flag0 : word;
     end;

{ Padding for future expansion  }
   PGtkStatusbarClass = ^TGtkStatusbarClass;
   TGtkStatusbarClass = record
        parent_class : TGtkHBoxClass;
        messages_mem_chunk : PGMemChunk;
        text_pushed : procedure (statusbar:PGtkStatusbar; context_id:guint; text:Pgchar); cdecl;
        text_popped : procedure (statusbar:PGtkStatusbar; context_id:guint; text:Pgchar); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_STATUSBAR : GType;
function GTK_STATUSBAR(obj: pointer) : PGtkStatusbar;
function GTK_STATUSBAR_CLASS(klass: pointer) : PGtkStatusbarClass;
function GTK_IS_STATUSBAR(obj: pointer) : boolean;
function GTK_IS_STATUSBAR_CLASS(klass: pointer) : boolean;
function GTK_STATUSBAR_GET_CLASS(obj: pointer) : PGtkStatusbarClass;

const
   bm_TGtkStatusbar_has_resize_grip = $1;
   bp_TGtkStatusbar_has_resize_grip = 0;
function has_resize_grip(var a : TGtkStatusbar) : guint;
procedure set_has_resize_grip(var a : TGtkStatusbar; __has_resize_grip : guint);

function gtk_statusbar_get_type:TGtkType; cdecl; external gtklib;
function gtk_statusbar_new:PGtkWidget; cdecl; external gtklib;
{ If you don't want to use contexts, 0 is a predefined global
   context_id you can pass to push/pop/remove
  }
function gtk_statusbar_get_context_id(statusbar:PGtkStatusbar; context_description:Pgchar):guint; cdecl; external gtklib;
{ Returns message_id used for gtk_statusbar_remove  }
function gtk_statusbar_push(statusbar:PGtkStatusbar; context_id:guint; text:Pgchar):guint; cdecl; external gtklib;
procedure gtk_statusbar_pop(statusbar:PGtkStatusbar; context_id:guint); cdecl; external gtklib;
procedure gtk_statusbar_remove(statusbar:PGtkStatusbar; context_id:guint; message_id:guint); cdecl; external gtklib;
procedure gtk_statusbar_set_has_resize_grip(statusbar:PGtkStatusbar; setting:gboolean); cdecl; external gtklib;
function gtk_statusbar_get_has_resize_grip(statusbar:PGtkStatusbar):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_STATUSBAR : GType;
begin
   GTK_TYPE_STATUSBAR:=gtk_statusbar_get_type;
end;

function GTK_STATUSBAR(obj: pointer) : PGtkStatusbar;
begin
   GTK_STATUSBAR:=PGtkStatusbar(GTK_CHECK_CAST(obj,GTK_TYPE_STATUSBAR));
end;

function GTK_STATUSBAR_CLASS(klass: pointer) : PGtkStatusbarClass;
begin
   GTK_STATUSBAR_CLASS:=PGtkStatusbarClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_STATUSBAR));
end;

function GTK_IS_STATUSBAR(obj: pointer) : boolean;
begin
   GTK_IS_STATUSBAR:=GTK_CHECK_TYPE(obj,GTK_TYPE_STATUSBAR);
end;

function GTK_IS_STATUSBAR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_STATUSBAR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_STATUSBAR);
end;

function GTK_STATUSBAR_GET_CLASS(obj: pointer) : PGtkStatusbarClass;
begin
   GTK_STATUSBAR_GET_CLASS:=PGtkStatusbarClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_STATUSBAR));
end;

function has_resize_grip(var a : TGtkStatusbar) : guint;
begin
   has_resize_grip:=(a.flag0 and bm_TGtkStatusbar_has_resize_grip) shr bp_TGtkStatusbar_has_resize_grip;
end;

procedure set_has_resize_grip(var a : TGtkStatusbar; __has_resize_grip : guint);
begin
   a.flag0:=a.flag0 or ((__has_resize_grip shl bp_TGtkStatusbar_has_resize_grip) and bm_TGtkStatusbar_has_resize_grip);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

