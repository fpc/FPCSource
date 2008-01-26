{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkSocket = ^TGtkSocket;
     TGtkSocket = record
          container : TGtkContainer;
          request_width : guint16;
          request_height : guint16;
          current_width : guint16;
          current_height : guint16;
          plug_window : PGdkWindow;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;

  const
     bm_TGtkSocket_same_app = $1;
     bp_TGtkSocket_same_app = 0;
     bm_TGtkSocket_focus_in = $2;
     bp_TGtkSocket_focus_in = 1;
     bm_TGtkSocket_have_size = $4;
     bp_TGtkSocket_have_size = 2;
     bm_TGtkSocket_need_map = $8;
     bp_TGtkSocket_need_map = 3;
function  same_app(var a : TGtkSocket) : guint;
procedure set_same_app(var a : TGtkSocket; __same_app : guint);
function  focus_in(var a : TGtkSocket) : guint;
procedure set_focus_in(var a : TGtkSocket; __focus_in : guint);
function  have_size(var a : TGtkSocket) : guint;
procedure set_have_size(var a : TGtkSocket; __have_size : guint);
function  need_map(var a : TGtkSocket) : guint;
procedure set_need_map(var a : TGtkSocket; __need_map : guint);

  type
     PGtkSocketClass = ^TGtkSocketClass;
     TGtkSocketClass = record
          parent_class : TGtkContainerClass;
       end;

type
  GTK_SOCKET=PGtkSocket;
  GTK_SOCKET_CLASS=PGtkSocket;

function  gtk_socket_new:PGtkWidget;cdecl;external gtkdll name 'gtk_socket_new';
function  gtk_socket_get_type:guint;cdecl;external gtkdll name 'gtk_socket_get_type';
procedure gtk_socket_steal(socket:PGtkSocket; wid:guint32);cdecl;external gtkdll name 'gtk_socket_steal';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  same_app(var a : TGtkSocket) : guint;
    begin
       same_app:=(a.flag0 and bm_TGtkSocket_same_app) shr bp_TGtkSocket_same_app;
    end;

procedure set_same_app(var a : TGtkSocket; __same_app : guint);
    begin
       a.flag0:=a.flag0 or ((__same_app shl bp_TGtkSocket_same_app) and bm_TGtkSocket_same_app);
    end;

function  focus_in(var a : TGtkSocket) : guint;
    begin
       focus_in:=(a.flag0 and bm_TGtkSocket_focus_in) shr bp_TGtkSocket_focus_in;
    end;

procedure set_focus_in(var a : TGtkSocket; __focus_in : guint);
    begin
       a.flag0:=a.flag0 or ((__focus_in shl bp_TGtkSocket_focus_in) and bm_TGtkSocket_focus_in);
    end;

function  have_size(var a : TGtkSocket) : guint;
    begin
       have_size:=(a.flag0 and bm_TGtkSocket_have_size) shr bp_TGtkSocket_have_size;
    end;

procedure set_have_size(var a : TGtkSocket; __have_size : guint);
    begin
       a.flag0:=a.flag0 or ((__have_size shl bp_TGtkSocket_have_size) and bm_TGtkSocket_have_size);
    end;

function  need_map(var a : TGtkSocket) : guint;
    begin
       need_map:=(a.flag0 and bm_TGtkSocket_need_map) shr bp_TGtkSocket_need_map;
    end;

procedure set_need_map(var a : TGtkSocket; __need_map : guint);
    begin
       a.flag0:=a.flag0 or ((__need_map shl bp_TGtkSocket_need_map) and bm_TGtkSocket_need_map);
    end;

{$endif read_implementation}


