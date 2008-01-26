{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkPaned = ^TGtkPaned;
     TGtkPaned = record
          container : TGtkContainer;
          child1 : PGtkWidget;
          child2 : PGtkWidget;
          handle : PGdkWindow;
          groove_rectangle : TGdkRectangle;
          xor_gc : PGdkGC;
          handle_size : guint16;
          gutter_size : guint16;
          child1_size : gint;
          last_allocation : gint;
          min_position : gint;
          max_position : gint;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          handle_xpos : gint16;
          handle_ypos : gint16;
       end;

  const
     bm_TGtkPaned_position_set = $1;
     bp_TGtkPaned_position_set = 0;
     bm_TGtkPaned_in_drag = $2;
     bp_TGtkPaned_in_drag = 1;
     bm_TGtkPaned_child1_shrink = $4;
     bp_TGtkPaned_child1_shrink = 2;
     bm_TGtkPaned_child1_resize = $8;
     bp_TGtkPaned_child1_resize = 3;
     bm_TGtkPaned_child2_shrink = $10;
     bp_TGtkPaned_child2_shrink = 4;
     bm_TGtkPaned_child2_resize = $20;
     bp_TGtkPaned_child2_resize = 5;
function  position_set(var a : TGtkPaned) : guint;
procedure set_position_set(var a : TGtkPaned; __position_set : guint);
function  in_drag(var a : TGtkPaned) : guint;
procedure set_in_drag(var a : TGtkPaned; __in_drag : guint);
function  child1_shrink(var a : TGtkPaned) : guint;
procedure set_child1_shrink(var a : TGtkPaned; __child1_shrink : guint);
function  child1_resize(var a : TGtkPaned) : guint;
procedure set_child1_resize(var a : TGtkPaned; __child1_resize : guint);
function  child2_shrink(var a : TGtkPaned) : guint;
procedure set_child2_shrink(var a : TGtkPaned; __child2_shrink : guint);
function  child2_resize(var a : TGtkPaned) : guint;
procedure set_child2_resize(var a : TGtkPaned; __child2_resize : guint);

  type
     PGtkPanedClass = ^TGtkPanedClass;
     TGtkPanedClass = record
          parent_class : TGtkContainerClass;
       end;

Type
  GTK_PANED=PGtkPaned;
  GTK_PANED_CLASS=PGtkPanedClass;

function  GTK_PANED_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_paned_get_type';
function  GTK_IS_PANED(obj:pointer):boolean;
function  GTK_IS_PANED_CLASS(klass:pointer):boolean;

function  gtk_paned_get_type:TGtkType;cdecl;external gtkdll name 'gtk_paned_get_type';
procedure gtk_paned_add1(paned:PGtkPaned; child:PGtkWidget);cdecl;external gtkdll name 'gtk_paned_add1';
procedure gtk_paned_add2(paned:PGtkPaned; child:PGtkWidget);cdecl;external gtkdll name 'gtk_paned_add2';
procedure gtk_paned_pack1(paned:PGtkPaned; child:PGtkWidget; resize:gboolean; shrink:gboolean);cdecl;external gtkdll name 'gtk_paned_pack1';
procedure gtk_paned_pack2(paned:PGtkPaned; child:PGtkWidget; resize:gboolean; shrink:gboolean);cdecl;external gtkdll name 'gtk_paned_pack2';
procedure gtk_paned_set_position(paned:PGtkPaned; position:gint);cdecl;external gtkdll name 'gtk_paned_set_position';
procedure gtk_paned_set_handle_size(paned:PGtkPaned; size:guint16);cdecl;external gtkdll name 'gtk_paned_set_handle_size';
{$ifndef gtkwin}
procedure gtk_paned_set_gutter_size(paned:PGtkPaned; size:guint16);cdecl;external gtkdll name 'gtk_paned_set_gutter_size';
{$endif}
procedure gtk_paned_compute_position(paned:PGtkPaned; allocation:gint; child1_req:gint; child2_req:gint);cdecl;external gtkdll name 'gtk_paned_compute_position';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  position_set(var a : TGtkPaned) : guint;
    begin
       position_set:=(a.flag0 and bm_TGtkPaned_position_set) shr bp_TGtkPaned_position_set;
    end;

procedure set_position_set(var a : TGtkPaned; __position_set : guint);
    begin
       a.flag0:=a.flag0 or ((__position_set shl bp_TGtkPaned_position_set) and bm_TGtkPaned_position_set);
    end;

function  in_drag(var a : TGtkPaned) : guint;
    begin
       in_drag:=(a.flag0 and bm_TGtkPaned_in_drag) shr bp_TGtkPaned_in_drag;
    end;

procedure set_in_drag(var a : TGtkPaned; __in_drag : guint);
    begin
       a.flag0:=a.flag0 or ((__in_drag shl bp_TGtkPaned_in_drag) and bm_TGtkPaned_in_drag);
    end;

function  child1_shrink(var a : TGtkPaned) : guint;
    begin
       child1_shrink:=(a.flag0 and bm_TGtkPaned_child1_shrink) shr bp_TGtkPaned_child1_shrink;
    end;

procedure set_child1_shrink(var a : TGtkPaned; __child1_shrink : guint);
    begin
       a.flag0:=a.flag0 or ((__child1_shrink shl bp_TGtkPaned_child1_shrink) and bm_TGtkPaned_child1_shrink);
    end;

function  child1_resize(var a : TGtkPaned) : guint;
    begin
       child1_resize:=(a.flag0 and bm_TGtkPaned_child1_resize) shr bp_TGtkPaned_child1_resize;
    end;

procedure set_child1_resize(var a : TGtkPaned; __child1_resize : guint);
    begin
       a.flag0:=a.flag0 or ((__child1_resize shl bp_TGtkPaned_child1_resize) and bm_TGtkPaned_child1_resize);
    end;

function  child2_shrink(var a : TGtkPaned) : guint;
    begin
       child2_shrink:=(a.flag0 and bm_TGtkPaned_child2_shrink) shr bp_TGtkPaned_child2_shrink;
    end;

procedure set_child2_shrink(var a : TGtkPaned; __child2_shrink : guint);
    begin
       a.flag0:=a.flag0 or ((__child2_shrink shl bp_TGtkPaned_child2_shrink) and bm_TGtkPaned_child2_shrink);
    end;

function  child2_resize(var a : TGtkPaned) : guint;
    begin
       child2_resize:=(a.flag0 and bm_TGtkPaned_child2_resize) shr bp_TGtkPaned_child2_resize;
    end;

procedure set_child2_resize(var a : TGtkPaned; __child2_resize : guint);
    begin
       a.flag0:=a.flag0 or ((__child2_resize shl bp_TGtkPaned_child2_resize) and bm_TGtkPaned_child2_resize);
    end;

function  GTK_IS_PANED(obj:pointer):boolean;
begin
  GTK_IS_PANED:=(obj<>nil) and GTK_IS_PANED_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_PANED_CLASS(klass:pointer):boolean;
begin
  GTK_IS_PANED_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_PANED_TYPE);
end;

{$endif read_implementation}


