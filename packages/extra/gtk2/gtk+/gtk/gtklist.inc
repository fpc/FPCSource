// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkList = ^TGtkList;
   TGtkList = record
        container : TGtkContainer;
        children : PGList;
        selection : PGList;
        undo_selection : PGList;
        undo_unselection : PGList;
        last_focus_child : PGtkWidget;
        undo_focus_child : PGtkWidget;
        htimer : guint;
        vtimer : guint;
        anchor : gint;
        drag_pos : gint;
        anchor_state : TGtkStateType;
        flag0 : word;
     end;


   PGtkListClass = ^TGtkListClass;
   TGtkListClass = record
        parent_class : TGtkContainerClass;
        selection_changed : procedure (list:PGtkList); cdecl;
        select_child : procedure (list:PGtkList; child:PGtkWidget); cdecl;
        unselect_child : procedure (list:PGtkList; child:PGtkWidget); cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkList_selection_mode = $3;
   bp_TGtkList_selection_mode = 0;
   bm_TGtkList_drag_selection = $4;
   bp_TGtkList_drag_selection = 2;
   bm_TGtkList_add_mode = $8;
   bp_TGtkList_add_mode = 3;


function GTK_TYPE_LIST : GType;
function GTK_LIST(obj: pointer) : PGtkList;
function GTK_LIST_CLASS(klass: pointer) : PGtkListClass;
function GTK_IS_LIST(obj: pointer) : boolean;
function GTK_IS_LIST_CLASS(klass: pointer) : boolean;
function GTK_LIST_GET_CLASS(obj: pointer) : PGtkListClass;


function selection_mode(var a : TGtkList) : guint; overload;
procedure set_selection_mode(var a : TGtkList; __selection_mode : guint); overload;
function drag_selection(var a : TGtkList) : guint;
procedure set_drag_selection(var a : TGtkList; __drag_selection : guint);
function add_mode(var a : TGtkList) : guint;
procedure set_add_mode(var a : TGtkList; __add_mode : guint);

function gtk_list_get_type:TGtkType; cdecl; external gtklib;
function gtk_list_new:PGtkWidget; cdecl; external gtklib;
procedure gtk_list_insert_items(list:PGtkList; items:PGList; position:gint); cdecl; external gtklib;
procedure gtk_list_append_items(list:PGtkList; items:PGList); cdecl; external gtklib;
procedure gtk_list_prepend_items(list:PGtkList; items:PGList); cdecl; external gtklib;
procedure gtk_list_remove_items(list:PGtkList; items:PGList); cdecl; external gtklib;
procedure gtk_list_remove_items_no_unref(list:PGtkList; items:PGList); cdecl; external gtklib;
procedure gtk_list_clear_items(list:PGtkList; start:gint; theEnd:gint); cdecl; external gtklib;
procedure gtk_list_select_item(list:PGtkList; item:gint); cdecl; external gtklib;
procedure gtk_list_unselect_item(list:PGtkList; item:gint); cdecl; external gtklib;
procedure gtk_list_select_child(list:PGtkList; child:PGtkWidget); cdecl; external gtklib;
procedure gtk_list_unselect_child(list:PGtkList; child:PGtkWidget); cdecl; external gtklib;
function gtk_list_child_position(list:PGtkList; child:PGtkWidget):gint; cdecl; external gtklib;
procedure gtk_list_set_selection_mode(list:PGtkList; mode:TGtkSelectionMode); cdecl; external gtklib;
procedure gtk_list_extend_selection(list:PGtkList; scroll_type:TGtkScrollType; position:gfloat; auto_start_selection:gboolean); cdecl; external gtklib;
procedure gtk_list_start_selection(list:PGtkList); cdecl; external gtklib;
procedure gtk_list_end_selection(list:PGtkList); cdecl; external gtklib;
procedure gtk_list_select_all(list:PGtkList); cdecl; external gtklib;
procedure gtk_list_unselect_all(list:PGtkList); cdecl; external gtklib;
procedure gtk_list_scroll_horizontal(list:PGtkList; scroll_type:TGtkScrollType; position:gfloat); cdecl; external gtklib;
procedure gtk_list_scroll_vertical(list:PGtkList; scroll_type:TGtkScrollType; position:gfloat); cdecl; external gtklib;
procedure gtk_list_toggle_add_mode(list:PGtkList); cdecl; external gtklib;
procedure gtk_list_toggle_focus_row(list:PGtkList); cdecl; external gtklib;
procedure gtk_list_toggle_row(list:PGtkList; item:PGtkWidget); cdecl; external gtklib;
procedure gtk_list_undo_selection(list:PGtkList); cdecl; external gtklib;
procedure gtk_list_end_drag_selection(list:PGtkList); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_LIST : GType;
begin
   GTK_TYPE_LIST:=gtk_list_get_type;
end;

function GTK_LIST(obj: pointer) : PGtkList;
begin
   GTK_LIST:=PGtkList(GTK_CHECK_CAST(obj,GTK_TYPE_LIST));
end;

function GTK_LIST_CLASS(klass: pointer) : PGtkListClass;
begin
   GTK_LIST_CLASS:=PGtkListClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_LIST));
end;

function GTK_IS_LIST(obj: pointer) : boolean;
begin
   GTK_IS_LIST:=GTK_CHECK_TYPE(obj,GTK_TYPE_LIST);
end;

function GTK_IS_LIST_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_LIST_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_LIST);
end;

function GTK_LIST_GET_CLASS(obj: pointer) : PGtkListClass;
begin
   GTK_LIST_GET_CLASS:=PGtkListClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_LIST));
end;

function selection_mode(var a : TGtkList) : guint;
begin
   selection_mode:=(a.flag0 and bm_TGtkList_selection_mode) shr bp_TGtkList_selection_mode;
end;

procedure set_selection_mode(var a : TGtkList; __selection_mode : guint);
begin
   a.flag0:=a.flag0 or ((__selection_mode shl bp_TGtkList_selection_mode) and bm_TGtkList_selection_mode);
end;

function drag_selection(var a : TGtkList) : guint;
begin
   drag_selection:=(a.flag0 and bm_TGtkList_drag_selection) shr bp_TGtkList_drag_selection;
end;

procedure set_drag_selection(var a : TGtkList; __drag_selection : guint);
begin
   a.flag0:=a.flag0 or ((__drag_selection shl bp_TGtkList_drag_selection) and bm_TGtkList_drag_selection);
end;

function add_mode(var a : TGtkList) : guint;
begin
   add_mode:=(a.flag0 and bm_TGtkList_add_mode) shr bp_TGtkList_add_mode;
end;

procedure set_add_mode(var a : TGtkList; __add_mode : guint);
begin
   a.flag0:=a.flag0 or ((__add_mode shl bp_TGtkList_add_mode) and bm_TGtkList_add_mode);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

