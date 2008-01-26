{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
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
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;

  const
     bm_TGtkList_selection_mode = $3;
     bp_TGtkList_selection_mode = 0;
     bm_TGtkList_drag_selection = $4;
     bp_TGtkList_drag_selection = 2;
     bm_TGtkList_add_mode = $8;
     bp_TGtkList_add_mode = 3;
function  selection_mode(var a : TGtkList) : guint;
procedure set_selection_mode(var a : TGtkList; __selection_mode : guint);
function  drag_selection(var a : TGtkList) : guint;cdecl;
procedure set_drag_selection(var a : TGtkList; __drag_selection : guint);cdecl;
function  add_mode(var a : TGtkList) : guint;
procedure set_add_mode(var a : TGtkList; __add_mode : guint);

  type
     PGtkListClass = ^TGtkListClass;
     TGtkListClass = record
          parent_class : TGtkContainerClass;
          selection_changed : procedure (list:PGtkList); cdecl;
          select_child : procedure (list:PGtkList; child:PGtkWidget); cdecl;
          unselect_child : procedure (list:PGtkList; child:PGtkWidget); cdecl;
       end;

Type
  GTK_LIST=PGtkList;
  GTK_LIST_CLASS=PGtkListClass;

function  GTK_LIST_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_list_get_type';
function  GTK_IS_LIST(obj:pointer):boolean;
function  GTK_IS_LIST_CLASS(klass:pointer):boolean;

function  gtk_list_get_type:TGtkType;cdecl;external gtkdll name 'gtk_list_get_type';
function  gtk_list_new:PGtkWidget;cdecl;external gtkdll name 'gtk_list_new';
procedure gtk_list_insert_items(list:PGtkList; items:PGList; position:gint);cdecl;external gtkdll name 'gtk_list_insert_items';
procedure gtk_list_append_items(list:PGtkList; items:PGList);cdecl;external gtkdll name 'gtk_list_append_items';
procedure gtk_list_prepend_items(list:PGtkList; items:PGList);cdecl;external gtkdll name 'gtk_list_prepend_items';
procedure gtk_list_remove_items(list:PGtkList; items:PGList);cdecl;external gtkdll name 'gtk_list_remove_items';
procedure gtk_list_remove_items_no_unref(list:PGtkList; items:PGList);cdecl;external gtkdll name 'gtk_list_remove_items_no_unref';
procedure gtk_list_clear_items(list:PGtkList; start:gint; theend:gint);cdecl;external gtkdll name 'gtk_list_clear_items';
procedure gtk_list_select_item(list:PGtkList; item:gint);cdecl;external gtkdll name 'gtk_list_select_item';
procedure gtk_list_unselect_item(list:PGtkList; item:gint);cdecl;external gtkdll name 'gtk_list_unselect_item';
procedure gtk_list_select_child(list:PGtkList; child:PGtkWidget);cdecl;external gtkdll name 'gtk_list_select_child';
procedure gtk_list_unselect_child(list:PGtkList; child:PGtkWidget);cdecl;external gtkdll name 'gtk_list_unselect_child';
function  gtk_list_child_position(list:PGtkList; child:PGtkWidget):gint;cdecl;external gtkdll name 'gtk_list_child_position';
procedure gtk_list_set_selection_mode(list:PGtkList; mode:TGtkSelectionMode);cdecl;external gtkdll name 'gtk_list_set_selection_mode';
procedure gtk_list_extend_selection(list:PGtkList; scroll_type:TGtkScrollType; position:gfloat; auto_start_selection:gboolean);cdecl;external gtkdll name 'gtk_list_extend_selection';
procedure gtk_list_start_selection(list:PGtkList);cdecl;external gtkdll name 'gtk_list_start_selection';
procedure gtk_list_end_selection(list:PGtkList);cdecl;external gtkdll name 'gtk_list_end_selection';
procedure gtk_list_select_all(list:PGtkList);cdecl;external gtkdll name 'gtk_list_select_all';
procedure gtk_list_unselect_all(list:PGtkList);cdecl;external gtkdll name 'gtk_list_unselect_all';
procedure gtk_list_scroll_horizontal(list:PGtkList; scroll_type:TGtkScrollType; position:gfloat);cdecl;external gtkdll name 'gtk_list_scroll_horizontal';
procedure gtk_list_scroll_vertical(list:PGtkList; scroll_type:TGtkScrollType; position:gfloat);cdecl;external gtkdll name 'gtk_list_scroll_vertical';
procedure gtk_list_toggle_add_mode(list:PGtkList);cdecl;external gtkdll name 'gtk_list_toggle_add_mode';
procedure gtk_list_toggle_focus_row(list:PGtkList);cdecl;external gtkdll name 'gtk_list_toggle_focus_row';
procedure gtk_list_toggle_row(list:PGtkList; item:PGtkWidget);cdecl;external gtkdll name 'gtk_list_toggle_row';
procedure gtk_list_undo_selection(list:PGtkList);cdecl;external gtkdll name 'gtk_list_undo_selection';
procedure gtk_list_end_drag_selection(list:PGtkList);cdecl;external gtkdll name 'gtk_list_end_drag_selection';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  selection_mode(var a : TGtkList) : guint;
    begin
       selection_mode:=(a.flag0 and bm_TGtkList_selection_mode) shr bp_TGtkList_selection_mode;
    end;

procedure set_selection_mode(var a : TGtkList; __selection_mode : guint);
    begin
       a.flag0:=a.flag0 or ((__selection_mode shl bp_TGtkList_selection_mode) and bm_TGtkList_selection_mode);
    end;

function  drag_selection(var a : TGtkList) : guint;cdecl;
    begin
       drag_selection:=(a.flag0 and bm_TGtkList_drag_selection) shr bp_TGtkList_drag_selection;
    end;

procedure set_drag_selection(var a : TGtkList; __drag_selection : guint);cdecl;
    begin
       a.flag0:=a.flag0 or ((__drag_selection shl bp_TGtkList_drag_selection) and bm_TGtkList_drag_selection);
    end;

function  add_mode(var a : TGtkList) : guint;
    begin
       add_mode:=(a.flag0 and bm_TGtkList_add_mode) shr bp_TGtkList_add_mode;
    end;

procedure set_add_mode(var a : TGtkList; __add_mode : guint);
    begin
       a.flag0:=a.flag0 or ((__add_mode shl bp_TGtkList_add_mode) and bm_TGtkList_add_mode);
    end;

function  GTK_IS_LIST(obj:pointer):boolean;
begin
  GTK_IS_LIST:=(obj<>nil) and GTK_IS_LIST_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_LIST_CLASS(klass:pointer):boolean;
begin
  GTK_IS_LIST_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_LIST_TYPE);
end;

{$endif read_implementation}


