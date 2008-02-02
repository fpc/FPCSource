{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkHandleBox = ^TGtkHandleBox;
     TGtkHandleBox = record
          bin : TGtkBin;
          bin_window : PGdkWindow;
          float_window : PGdkWindow;
          shadow_type : TGtkShadowType;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          deskoff_x : gint;
          deskoff_y : gint;
          attach_allocation : TGtkAllocation;
          float_allocation : TGtkAllocation;
       end;

  const
     bm_TGtkHandleBox_handle_position = $3;
     bp_TGtkHandleBox_handle_position = 0;
     bm_TGtkHandleBox_float_window_mapped = $4;
     bp_TGtkHandleBox_float_window_mapped = 2;
     bm_TGtkHandleBox_child_detached = $8;
     bp_TGtkHandleBox_child_detached = 3;
     bm_TGtkHandleBox_in_drag = $10;
     bp_TGtkHandleBox_in_drag = 4;
     bm_TGtkHandleBox_shrink_on_detach = $20;
     bp_TGtkHandleBox_shrink_on_detach = 5;
     bm_TGtkHandleBox_snap_edge = $1C0;
     bp_TGtkHandleBox_snap_edge = 6;
function  handle_position(var a : TGtkHandleBox) : guint;
procedure set_handle_position(var a : TGtkHandleBox; __handle_position : guint);
function  float_window_mapped(var a : TGtkHandleBox) : guint;
procedure set_float_window_mapped(var a : TGtkHandleBox; __float_window_mapped : guint);
function  child_detached(var a : TGtkHandleBox) : guint;
procedure set_child_detached(var a : TGtkHandleBox; __child_detached : guint);
function  in_drag(var a : TGtkHandleBox) : guint;
procedure set_in_drag(var a : TGtkHandleBox; __in_drag : guint);
function  shrink_on_detach(var a : TGtkHandleBox) : guint;
procedure set_shrink_on_detach(var a : TGtkHandleBox; __shrink_on_detach : guint);
function  snap_edge(var a : TGtkHandleBox) : gint;
procedure set_snap_edge(var a : TGtkHandleBox; __snap_edge : gint);

  type
     PGtkHandleBoxClass = ^TGtkHandleBoxClass;
     TGtkHandleBoxClass = record
          parent_class : TGtkBinClass;
          child_attached : procedure (handle_box:PGtkHandleBox; child:PGtkWidget); cdecl;
          child_detached : procedure (handle_box:PGtkHandleBox; child:PGtkWidget); cdecl;
       end;

Type
  GTK_HANDLE_BOX=PGtkHandleBox;
  GTK_HANDLE_BOX_CLASS=PGtkHandleBoxClass;

function  GTK_HANDLE_BOX_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_handle_box_get_type';
function  GTK_IS_HANDLE_BOX(obj:pointer):boolean;
function  GTK_IS_HANDLE_BOX_CLASS(klass:pointer):boolean;

function  gtk_handle_box_get_type:TGtkType;cdecl;external gtkdll name 'gtk_handle_box_get_type';
function  gtk_handle_box_new : PGtkWidget;cdecl;external gtkdll name 'gtk_handle_box_new';
procedure gtk_handle_box_set_shadow_type(handle_box:PGtkHandleBox; thetype:TGtkShadowType);cdecl;external gtkdll name 'gtk_handle_box_set_shadow_type';
procedure gtk_handle_box_set_handle_position(handle_box:PGtkHandleBox; position:TGtkPositionType);cdecl;external gtkdll name 'gtk_handle_box_set_handle_position';
procedure gtk_handle_box_set_snap_edge(handle_box:PGtkHandleBox; edge:TGtkPositionType);cdecl;external gtkdll name 'gtk_handle_box_set_snap_edge';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  handle_position(var a : TGtkHandleBox) : guint;
    begin
       handle_position:=(a.flag0 and bm_TGtkHandleBox_handle_position) shr bp_TGtkHandleBox_handle_position;
    end;

procedure set_handle_position(var a : TGtkHandleBox; __handle_position : guint);
    begin
       a.flag0:=a.flag0 or ((__handle_position shl bp_TGtkHandleBox_handle_position) and bm_TGtkHandleBox_handle_position);
    end;

function  float_window_mapped(var a : TGtkHandleBox) : guint;
    begin
       float_window_mapped:=(a.flag0 and bm_TGtkHandleBox_float_window_mapped) shr bp_TGtkHandleBox_float_window_mapped;
    end;

procedure set_float_window_mapped(var a : TGtkHandleBox; __float_window_mapped : guint);
    begin
       a.flag0:=a.flag0 or ((__float_window_mapped shl bp_TGtkHandleBox_float_window_mapped) and bm_TGtkHandleBox_float_window_mapped);
    end;

function  child_detached(var a : TGtkHandleBox) : guint;
    begin
       child_detached:=(a.flag0 and bm_TGtkHandleBox_child_detached) shr bp_TGtkHandleBox_child_detached;
    end;

procedure set_child_detached(var a : TGtkHandleBox; __child_detached : guint);
    begin
       a.flag0:=a.flag0 or ((__child_detached shl bp_TGtkHandleBox_child_detached) and bm_TGtkHandleBox_child_detached);
    end;

function  in_drag(var a : TGtkHandleBox) : guint;
    begin
       in_drag:=(a.flag0 and bm_TGtkHandleBox_in_drag) shr bp_TGtkHandleBox_in_drag;
    end;

procedure set_in_drag(var a : TGtkHandleBox; __in_drag : guint);
    begin
       a.flag0:=a.flag0 or ((__in_drag shl bp_TGtkHandleBox_in_drag) and bm_TGtkHandleBox_in_drag);
    end;

function  shrink_on_detach(var a : TGtkHandleBox) : guint;
    begin
       shrink_on_detach:=(a.flag0 and bm_TGtkHandleBox_shrink_on_detach) shr bp_TGtkHandleBox_shrink_on_detach;
    end;

procedure set_shrink_on_detach(var a : TGtkHandleBox; __shrink_on_detach : guint);
    begin
       a.flag0:=a.flag0 or ((__shrink_on_detach shl bp_TGtkHandleBox_shrink_on_detach) and bm_TGtkHandleBox_shrink_on_detach);
    end;

function  snap_edge(var a : TGtkHandleBox) : gint;
    begin
       snap_edge:=(a.flag0 and bm_TGtkHandleBox_snap_edge) shr bp_TGtkHandleBox_snap_edge;
    end;

procedure set_snap_edge(var a : TGtkHandleBox; __snap_edge : gint);
    begin
       a.flag0:=a.flag0 or ((__snap_edge shl bp_TGtkHandleBox_snap_edge) and bm_TGtkHandleBox_snap_edge);
    end;

function  GTK_IS_HANDLE_BOX(obj:pointer):boolean;
begin
  GTK_IS_HANDLE_BOX:=(obj<>nil) and GTK_IS_HANDLE_BOX_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_HANDLE_BOX_CLASS(klass:pointer):boolean;
begin
  GTK_IS_HANDLE_BOX_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_HANDLE_BOX_TYPE);
end;

{$endif read_implementation}


