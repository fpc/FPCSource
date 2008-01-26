// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkPlug = ^TGtkPlug;
   TGtkPlug = record
        window : TGtkWindow;
        socket_window : PGdkWindow;
        modality_window : PGtkWidget;
        modality_group : PGtkWindowGroup;
        grabbed_keys : PGHashTable;
        flag0 : word;
     end;

{ Padding for future expansion  }
   PGtkPlugClass = ^TGtkPlugClass;
   TGtkPlugClass = record
        parent_class : TGtkWindowClass;
        embedded : procedure (plug:PGtkPlug); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkPlug_same_app = $1;
   bp_TGtkPlug_same_app = 0;
function GTK_TYPE_PLUG : GType;
function GTK_PLUG(obj: pointer) : PGtkPlug;
function GTK_PLUG_CLASS(klass: pointer) : PGtkPlugClass;
function GTK_IS_PLUG(obj: pointer) : boolean;
function GTK_IS_PLUG_CLASS(klass: pointer) : boolean;
function GTK_PLUG_GET_CLASS(obj: pointer) : PGtkPlugClass;

function same_app(var a : TGtkPlug) : guint; overload;
procedure set_same_app(var a : TGtkPlug; __same_app : guint); overload;


function gtk_plug_get_type:TGtkType; cdecl; external gtklib;

{$ifndef GDK_MULTIHEAD_SAFE}
procedure gtk_plug_construct(plug:PGtkPlug; socket_id:TGdkNativeWindow); cdecl; external gtklib;
function gtk_plug_new(socket_id:TGdkNativeWindow):PGtkWidget; cdecl; external gtklib;
{$endif}

procedure gtk_plug_construct_for_display(plug:PGtkPlug; display:PGdkDisplay; socket_id:TGdkNativeWindow); cdecl; external gtklib;
function gtk_plug_new_for_display(display:PGdkDisplay; socket_id:TGdkNativeWindow):PGtkWidget; cdecl; external gtklib;
function gtk_plug_get_id(plug:PGtkPlug):TGdkNativeWindow; cdecl; external gtklib;
procedure _gtk_plug_add_to_socket(plug:PGtkPlug; socket:PGtkSocket); cdecl; external gtklib;
procedure _gtk_plug_remove_from_socket(plug:PGtkPlug; socket:PGtkSocket); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_PLUG : GType;
begin
   GTK_TYPE_PLUG:=gtk_plug_get_type;
end;

function GTK_PLUG(obj: pointer) : PGtkPlug;
begin
   GTK_PLUG:=PGtkPlug(GTK_CHECK_CAST(obj,GTK_TYPE_PLUG));
end;

function GTK_PLUG_CLASS(klass: pointer) : PGtkPlugClass;
begin
   GTK_PLUG_CLASS:=PGtkPlugClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_PLUG));
end;

function GTK_IS_PLUG(obj: pointer) : boolean;
begin
   GTK_IS_PLUG:=GTK_CHECK_TYPE(obj,GTK_TYPE_PLUG);
end;

function GTK_IS_PLUG_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_PLUG_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_PLUG);
end;

function GTK_PLUG_GET_CLASS(obj: pointer) : PGtkPlugClass;
begin
   GTK_PLUG_GET_CLASS:=PGtkPlugClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_PLUG));
end;

function same_app(var a : TGtkPlug) : guint;
begin
   same_app:=(a.flag0 and bm_TGtkPlug_same_app) shr bp_TGtkPlug_same_app;
end;

procedure set_same_app(var a : TGtkPlug; __same_app : guint);
begin
   a.flag0:=a.flag0 or ((__same_app shl bp_TGtkPlug_same_app) and bm_TGtkPlug_same_app);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

