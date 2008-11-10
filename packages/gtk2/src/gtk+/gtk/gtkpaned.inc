// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{< private > }
   PGtkPaned = ^TGtkPaned;
   TGtkPaned = record
        container : TGtkContainer;
        child1 : PGtkWidget;
        child2 : PGtkWidget;
        handle : PGdkWindow;
        xor_gc : PGdkGC;
        cursor_type : TGdkCursorType;
        handle_pos : TGdkRectangle;
        child1_size : gint;
        last_allocation : gint;
        min_position : gint;
        max_position : gint;
        flag0 : word;
        last_child1_focus : PGtkWidget;
        last_child2_focus : PGtkWidget;
        saved_focus : PGtkWidget;
        drag_pos : gint;
        original_position : gint;
     end;

{ Padding for future expansion  }
   PGtkPanedClass = ^TGtkPanedClass;
   TGtkPanedClass = record
        parent_class : TGtkContainerClass;
        cycle_child_focus : function (paned:PGtkPaned; reverse:gboolean):gboolean; cdecl;
        toggle_handle_focus : function (paned:PGtkPaned):gboolean; cdecl;
        move_handle : function (paned:PGtkPaned; scroll:TGtkScrollType):gboolean; cdecl;
        cycle_handle_focus : function (paned:PGtkPaned; reverse:gboolean):gboolean; cdecl;
        accept_position : function (paned:PGtkPaned):gboolean; cdecl;
        cancel_position : function (paned:PGtkPaned):gboolean; cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
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
   bm_TGtkPaned_orientation = $40;
   bp_TGtkPaned_orientation = 6;
   bm_TGtkPaned_in_recursion = $80;
   bp_TGtkPaned_in_recursion = 7;
   bm_TGtkPaned_handle_prelit = $100;
   bp_TGtkPaned_handle_prelit = 8;


function GTK_TYPE_PANED : GType;
function GTK_PANED(obj: pointer) : PGtkPaned;
function GTK_PANED_CLASS(klass: pointer) : PGtkPanedClass;
function GTK_IS_PANED(obj: pointer) : boolean;
function GTK_IS_PANED_CLASS(klass: pointer) : boolean;
function GTK_PANED_GET_CLASS(obj: pointer) : PGtkPanedClass;

function position_set(var a : TGtkPaned) : guint;
procedure set_position_set(var a : TGtkPaned; __position_set : guint);
function in_drag(var a : TGtkPaned) : guint; overload;
procedure set_in_drag(var a : TGtkPaned; __in_drag : guint); overload;
function child1_shrink(var a : TGtkPaned) : guint;
procedure set_child1_shrink(var a : TGtkPaned; __child1_shrink : guint);
function child1_resize(var a : TGtkPaned) : guint;
procedure set_child1_resize(var a : TGtkPaned; __child1_resize : guint);
function child2_shrink(var a : TGtkPaned) : guint;
procedure set_child2_shrink(var a : TGtkPaned; __child2_shrink : guint);
function child2_resize(var a : TGtkPaned) : guint;
procedure set_child2_resize(var a : TGtkPaned; __child2_resize : guint);
function orientation(var a : TGtkPaned) : guint;
procedure set_orientation(var a : TGtkPaned; __orientation : guint);
function in_recursion(var a : TGtkPaned) : guint;
procedure set_in_recursion(var a : TGtkPaned; __in_recursion : guint);
function handle_prelit(var a : TGtkPaned) : guint;
procedure set_handle_prelit(var a : TGtkPaned; __handle_prelit : guint);

function gtk_paned_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_paned_add1(paned:PGtkPaned; child:PGtkWidget); cdecl; external gtklib;
procedure gtk_paned_add2(paned:PGtkPaned; child:PGtkWidget); cdecl; external gtklib;
procedure gtk_paned_pack1(paned:PGtkPaned; child:PGtkWidget; resize:gboolean; shrink:gboolean); cdecl; external gtklib;
procedure gtk_paned_pack2(paned:PGtkPaned; child:PGtkWidget; resize:gboolean; shrink:gboolean); cdecl; external gtklib;
function gtk_paned_get_position(paned:PGtkPaned):gint; cdecl; external gtklib;
procedure gtk_paned_set_position(paned:PGtkPaned; position:gint); cdecl; external gtklib;

{ Internal function  }
procedure gtk_paned_compute_position(paned:PGtkPaned; allocation:gint; child1_req:gint; child2_req:gint); cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_paned_gutter_size(p : pointer; s: gint);
procedure gtk_paned_set_gutter_size(p: pointer; s : gint);
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ifdef HasGTK2_4}
function gtk_paned_get_child1(paned: PGtkPaned): PGtkWidget; cdecl; external gtklib;
function gtk_paned_get_child2(paned: PGtkPaned): PGtkWidget; cdecl; external gtklib;
{$endif}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_PANED : GType;
begin
   GTK_TYPE_PANED:=gtk_paned_get_type;
end;

function GTK_PANED(obj: pointer) : PGtkPaned;
begin
   GTK_PANED:=PGtkPaned(GTK_CHECK_CAST(obj,GTK_TYPE_PANED));
end;

function GTK_PANED_CLASS(klass: pointer) : PGtkPanedClass;
begin
   GTK_PANED_CLASS:=PGtkPanedClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_PANED));
end;

function GTK_IS_PANED(obj: pointer) : boolean;
begin
   GTK_IS_PANED:=GTK_CHECK_TYPE(obj,GTK_TYPE_PANED);
end;

function GTK_IS_PANED_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_PANED_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_PANED);
end;

function GTK_PANED_GET_CLASS(obj: pointer) : PGtkPanedClass;
begin
   GTK_PANED_GET_CLASS:=PGtkPanedClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_PANED));
end;

function position_set(var a : TGtkPaned) : guint;
begin
   position_set:=(a.flag0 and bm_TGtkPaned_position_set) shr bp_TGtkPaned_position_set;
end;

procedure set_position_set(var a : TGtkPaned; __position_set : guint);
begin
   a.flag0:=a.flag0 or ((__position_set shl bp_TGtkPaned_position_set) and bm_TGtkPaned_position_set);
end;

function in_drag(var a : TGtkPaned) : guint;
begin
   in_drag:=(a.flag0 and bm_TGtkPaned_in_drag) shr bp_TGtkPaned_in_drag;
end;

procedure set_in_drag(var a : TGtkPaned; __in_drag : guint);
begin
   a.flag0:=a.flag0 or ((__in_drag shl bp_TGtkPaned_in_drag) and bm_TGtkPaned_in_drag);
end;

function child1_shrink(var a : TGtkPaned) : guint;
begin
   child1_shrink:=(a.flag0 and bm_TGtkPaned_child1_shrink) shr bp_TGtkPaned_child1_shrink;
end;

procedure set_child1_shrink(var a : TGtkPaned; __child1_shrink : guint);
begin
   a.flag0:=a.flag0 or ((__child1_shrink shl bp_TGtkPaned_child1_shrink) and bm_TGtkPaned_child1_shrink);
end;

function child1_resize(var a : TGtkPaned) : guint;
begin
   child1_resize:=(a.flag0 and bm_TGtkPaned_child1_resize) shr bp_TGtkPaned_child1_resize;
end;

procedure set_child1_resize(var a : TGtkPaned; __child1_resize : guint);
begin
   a.flag0:=a.flag0 or ((__child1_resize shl bp_TGtkPaned_child1_resize) and bm_TGtkPaned_child1_resize);
end;

function child2_shrink(var a : TGtkPaned) : guint;
begin
   child2_shrink:=(a.flag0 and bm_TGtkPaned_child2_shrink) shr bp_TGtkPaned_child2_shrink;
end;

procedure set_child2_shrink(var a : TGtkPaned; __child2_shrink : guint);
begin
   a.flag0:=a.flag0 or ((__child2_shrink shl bp_TGtkPaned_child2_shrink) and bm_TGtkPaned_child2_shrink);
end;

function child2_resize(var a : TGtkPaned) : guint;
begin
   child2_resize:=(a.flag0 and bm_TGtkPaned_child2_resize) shr bp_TGtkPaned_child2_resize;
end;

procedure set_child2_resize(var a : TGtkPaned; __child2_resize : guint);
begin
   a.flag0:=a.flag0 or ((__child2_resize shl bp_TGtkPaned_child2_resize) and bm_TGtkPaned_child2_resize);
end;

function orientation(var a : TGtkPaned) : guint;
begin
   orientation:=(a.flag0 and bm_TGtkPaned_orientation) shr bp_TGtkPaned_orientation;
end;

procedure set_orientation(var a : TGtkPaned; __orientation : guint);
begin
   a.flag0:=a.flag0 or ((__orientation shl bp_TGtkPaned_orientation) and bm_TGtkPaned_orientation);
end;

function in_recursion(var a : TGtkPaned) : guint;
begin
   in_recursion:=(a.flag0 and bm_TGtkPaned_in_recursion) shr bp_TGtkPaned_in_recursion;
end;

procedure set_in_recursion(var a : TGtkPaned; __in_recursion : guint);
begin
   a.flag0:=a.flag0 or ((__in_recursion shl bp_TGtkPaned_in_recursion) and bm_TGtkPaned_in_recursion);
end;

function handle_prelit(var a : TGtkPaned) : guint;
begin
   handle_prelit:=(a.flag0 and bm_TGtkPaned_handle_prelit) shr bp_TGtkPaned_handle_prelit;
end;

procedure set_handle_prelit(var a : TGtkPaned; __handle_prelit : guint);
begin
   a.flag0:=a.flag0 or ((__handle_prelit shl bp_TGtkPaned_handle_prelit) and bm_TGtkPaned_handle_prelit);
end;

procedure gtk_paned_gutter_size(p : pointer; s: gint);
begin
   // Dummy statement
   if (p<>nil) and (s<>0) then ;
end;

procedure gtk_paned_set_gutter_size(p: pointer; s : gint);
begin
   // Dummy statement
   if (p<>nil) and (s<>0) then ;
end;

{$ENDIF read_implementation}
// included by gtk2.pas

