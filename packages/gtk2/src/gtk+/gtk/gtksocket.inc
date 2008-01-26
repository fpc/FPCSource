// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ -1 == not xembed  }
   PGtkSocket = ^TGtkSocket;
   TGtkSocket = record
        container : TGtkContainer;
        request_width : guint16;
        request_height : guint16;
        current_width : guint16;
        current_height : guint16;
        plug_window : PGdkWindow;
        plug_widget : PGtkWidget;
        xembed_version : gshort;
        flag0 : word;
        accel_group : PGtkAccelGroup;
        toplevel : PGtkWidget;
     end;


{ Padding for future expansion  }
   PGtkSocketClass = ^TGtkSocketClass;
   TGtkSocketClass = record
        parent_class : TGtkContainerClass;
        plug_added : procedure (socket:PGtkSocket); cdecl;
        plug_removed : function (socket:PGtkSocket):gboolean; cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkSocket_same_app = $1;
   bp_TGtkSocket_same_app = 0;
   bm_TGtkSocket_focus_in = $2;
   bp_TGtkSocket_focus_in = 1;
   bm_TGtkSocket_have_size = $4;
   bp_TGtkSocket_have_size = 2;
   bm_TGtkSocket_need_map = $8;
   bp_TGtkSocket_need_map = 3;
   bm_TGtkSocket_is_mapped = $10;
   bp_TGtkSocket_is_mapped = 4;


function GTK_TYPE_SOCKET : GType;
function GTK_SOCKET(obj: pointer) : PGtkSocket;
function GTK_SOCKET_CLASS(klass: pointer) : PGtkSocketClass;
function GTK_IS_SOCKET(obj: pointer) : boolean;
function GTK_IS_SOCKET_CLASS(klass: pointer) : boolean;
function GTK_SOCKET_GET_CLASS(obj: pointer) : PGtkSocketClass;


function same_app(var a : TGtkSocket) : guint; overload;
procedure set_same_app(var a : TGtkSocket; __same_app : guint); overload;
function focus_in(var a : TGtkSocket) : guint;
procedure set_focus_in(var a : TGtkSocket; __focus_in : guint);
function have_size(var a : TGtkSocket) : guint;
procedure set_have_size(var a : TGtkSocket; __have_size : guint);
function need_map(var a : TGtkSocket) : guint;
procedure set_need_map(var a : TGtkSocket; __need_map : guint);
function is_mapped(var a : TGtkSocket) : guint;
procedure set_is_mapped(var a : TGtkSocket; __is_mapped : guint);

function gtk_socket_new:PGtkWidget; cdecl; external gtklib;
function gtk_socket_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_socket_add_id(socket:PGtkSocket; window_id:TGdkNativeWindow); cdecl; external gtklib;
function gtk_socket_get_id(socket:PGtkSocket):TGdkNativeWindow; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_socket_steal(socket:PGtkSocket; wid:TGdkNativeWindow); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_SOCKET : GType;
begin
   GTK_TYPE_SOCKET:=gtk_socket_get_type;
end;

function GTK_SOCKET(obj: pointer) : PGtkSocket;
begin
   GTK_SOCKET:=PGtkSocket(GTK_CHECK_CAST(obj,GTK_TYPE_SOCKET));
end;

function GTK_SOCKET_CLASS(klass: pointer) : PGtkSocketClass;
begin
   GTK_SOCKET_CLASS:=PGtkSocketClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_SOCKET));
end;

function GTK_IS_SOCKET(obj: pointer) : boolean;
begin
   GTK_IS_SOCKET:=GTK_CHECK_TYPE(obj,GTK_TYPE_SOCKET);
end;

function GTK_IS_SOCKET_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_SOCKET_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_SOCKET);
end;

function GTK_SOCKET_GET_CLASS(obj: pointer) : PGtkSocketClass;
begin
   GTK_SOCKET_GET_CLASS:=PGtkSocketClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_SOCKET));
end;

function same_app(var a : TGtkSocket) : guint;
begin
   same_app:=(a.flag0 and bm_TGtkSocket_same_app) shr bp_TGtkSocket_same_app;
end;

procedure set_same_app(var a : TGtkSocket; __same_app : guint);
begin
   a.flag0:=a.flag0 or ((__same_app shl bp_TGtkSocket_same_app) and bm_TGtkSocket_same_app);
end;

function focus_in(var a : TGtkSocket) : guint;
begin
   focus_in:=(a.flag0 and bm_TGtkSocket_focus_in) shr bp_TGtkSocket_focus_in;
end;

procedure set_focus_in(var a : TGtkSocket; __focus_in : guint);
begin
   a.flag0:=a.flag0 or ((__focus_in shl bp_TGtkSocket_focus_in) and bm_TGtkSocket_focus_in);
end;

function have_size(var a : TGtkSocket) : guint;
begin
   have_size:=(a.flag0 and bm_TGtkSocket_have_size) shr bp_TGtkSocket_have_size;
end;

procedure set_have_size(var a : TGtkSocket; __have_size : guint);
begin
   a.flag0:=a.flag0 or ((__have_size shl bp_TGtkSocket_have_size) and bm_TGtkSocket_have_size);
end;

function need_map(var a : TGtkSocket) : guint;
begin
   need_map:=(a.flag0 and bm_TGtkSocket_need_map) shr bp_TGtkSocket_need_map;
end;

procedure set_need_map(var a : TGtkSocket; __need_map : guint);
begin
   a.flag0:=a.flag0 or ((__need_map shl bp_TGtkSocket_need_map) and bm_TGtkSocket_need_map);
end;

function is_mapped(var a : TGtkSocket) : guint;
begin
   is_mapped:=(a.flag0 and bm_TGtkSocket_is_mapped) shr bp_TGtkSocket_is_mapped;
end;

procedure set_is_mapped(var a : TGtkSocket; __is_mapped : guint);
begin
   a.flag0:=a.flag0 or ((__is_mapped shl bp_TGtkSocket_is_mapped) and bm_TGtkSocket_is_mapped);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

