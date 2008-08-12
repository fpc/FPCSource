// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkNotebookPage = pointer;

   PGtkNotebookTab = ^TGtkNotebookTab;
   TGtkNotebookTab = (
     GTK_NOTEBOOK_TAB_FIRST,
     GTK_NOTEBOOK_TAB_LAST
   );

{ The first tab visible (for scrolling notebooks)  }
{ Flag used by ::move-focus-out implementation  }
   PGtkNotebook = ^TGtkNotebook;
   TGtkNotebook = record
        container : TGtkContainer;
        cur_page : PGtkNotebookPage;
        children : PGList;
        first_tab : PGList;
        focus_tab : PGList;
        menu : PGtkWidget;
        event_window : PGdkWindow;
        timer : guint32;
        tab_hborder : guint16;
        tab_vborder : guint16;
        flag0 : word;
     end;

{ Action signals for keybindings  }
{ Padding for future expansion  }
   PGtkNotebookClass = ^TGtkNotebookClass;
   TGtkNotebookClass = record
        parent_class : TGtkContainerClass;
        switch_page : procedure (notebook:PGtkNotebook; page:PGtkNotebookPage; page_num:guint); cdecl;
        select_page : function (notebook:PGtkNotebook; move_focus:gboolean):gboolean; cdecl;
        focus_tab : function (notebook:PGtkNotebook; _type:TGtkNotebookTab):gboolean; cdecl;
        change_current_page : procedure (notebook:PGtkNotebook; offset:gint); cdecl;
        move_focus_out : procedure (notebook:PGtkNotebook; direction:TGtkDirectionType); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkNotebook_show_tabs = $1;
   bp_TGtkNotebook_show_tabs = 0;
   bm_TGtkNotebook_homogeneous = $2;
   bp_TGtkNotebook_homogeneous = 1;
   bm_TGtkNotebook_show_border = $4;
   bp_TGtkNotebook_show_border = 2;
   bm_TGtkNotebook_tab_pos = $18;
   bp_TGtkNotebook_tab_pos = 3;
   bm_TGtkNotebook_scrollable = $20;
   bp_TGtkNotebook_scrollable = 5;
   bm_TGtkNotebook_in_child = $C0;
   bp_TGtkNotebook_in_child = 6;
   bm_TGtkNotebook_click_child = $300;
   bp_TGtkNotebook_click_child = 8;
   bm_TGtkNotebook_button = $C00;
   bp_TGtkNotebook_button = 10;
   bm_TGtkNotebook_need_timer = $1000;
   bp_TGtkNotebook_need_timer = 12;
   bm_TGtkNotebook_child_has_focus = $2000;
   bp_TGtkNotebook_child_has_focus = 13;
   bm_TGtkNotebook_have_visible_child = $4000;
   bp_TGtkNotebook_have_visible_child = 14;
   bm_TGtkNotebook_focus_out = $8000;
   bp_TGtkNotebook_focus_out = 15;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_notebook_current_page(notebook:PGtkNotebook):gint;
procedure gtk_notebook_set_page(notebook:PGtkNotebook; page_num:gint);
{$endif}

function GTK_TYPE_NOTEBOOK : GType;
function GTK_NOTEBOOK(obj: pointer) : PGtkNotebook;
function GTK_NOTEBOOK_CLASS(klass: pointer) : PGtkNotebookClass;
function GTK_IS_NOTEBOOK(obj: pointer) : boolean;
function GTK_IS_NOTEBOOK_CLASS(klass: pointer) : boolean;
function GTK_NOTEBOOK_GET_CLASS(obj: pointer) : PGtkNotebookClass;

function show_tabs(var a : TGtkNotebook) : guint;
procedure set_show_tabs(var a : TGtkNotebook; __show_tabs : guint);
function homogeneous(var a : TGtkNotebook) : guint; overload;
procedure set_homogeneous(var a : TGtkNotebook; __homogeneous : guint); overload;
function show_border(var a : TGtkNotebook) : guint;
procedure set_show_border(var a : TGtkNotebook; __show_border : guint);
function tab_pos(var a : TGtkNotebook) : guint;
procedure set_tab_pos(var a : TGtkNotebook; __tab_pos : guint);
function scrollable(var a : TGtkNotebook) : guint;
procedure set_scrollable(var a : TGtkNotebook; __scrollable : guint);
function in_child(var a : TGtkNotebook) : guint; overload;
procedure set_in_child(var a : TGtkNotebook; __in_child : guint); overload;
function click_child(var a : TGtkNotebook) : guint; overload;
procedure set_click_child(var a : TGtkNotebook; __click_child : guint); overload;
function button(var a : TGtkNotebook) : guint; overload;
procedure set_button(var a : TGtkNotebook; __button : guint); overload;
function need_timer(var a : TGtkNotebook) : guint; overload;
procedure set_need_timer(var a : TGtkNotebook; __need_timer : guint); overload;
function child_has_focus(var a : TGtkNotebook) : guint;
procedure set_child_has_focus(var a : TGtkNotebook; __child_has_focus : guint);
function have_visible_child(var a : TGtkNotebook) : guint;
procedure set_have_visible_child(var a : TGtkNotebook; __have_visible_child : guint);
function focus_out(var a : TGtkNotebook) : guint; overload;
procedure set_focus_out(var a : TGtkNotebook; __focus_out : guint); overload;

{ Creation, insertion, deletion }
function gtk_notebook_get_type:TGtkType; cdecl; external gtklib;
function gtk_notebook_new:PGtkWidget; cdecl; external gtklib;
function gtk_notebook_append_page(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget): gint; cdecl; external gtklib;
function gtk_notebook_append_page_menu(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget; menu_label:PGtkWidget):gint; cdecl; external gtklib;
function gtk_notebook_prepend_page(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget):gint; cdecl; external gtklib;
function gtk_notebook_prepend_page_menu(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget; menu_label:PGtkWidget):gint;cdecl; external gtklib;
function gtk_notebook_insert_page(notebook:PGtkNotebook; child:PGtkWidget;tab_label:PGtkWidget; position:gint):gint; cdecl; external gtklib;
function gtk_notebook_insert_page_menu(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget; menu_label:PGtkWidget;position:gint):gint; cdecl; external gtklib;
procedure gtk_notebook_remove_page(notebook:PGtkNotebook; page_num:gint); cdecl; external gtklib;

{ query, set current NoteebookPage }
function gtk_notebook_get_current_page(notebook:PGtkNotebook):gint; cdecl; external gtklib;
function gtk_notebook_get_nth_page(notebook:PGtkNotebook; page_num:gint):PGtkWidget; cdecl; external gtklib;
function gtk_notebook_page_num(notebook:PGtkNotebook; child:PGtkWidget):gint; cdecl; external gtklib;
procedure gtk_notebook_set_current_page(notebook:PGtkNotebook; page_num:gint); cdecl; external gtklib;
procedure gtk_notebook_next_page(notebook:PGtkNotebook); cdecl; external gtklib;
procedure gtk_notebook_prev_page(notebook:PGtkNotebook); cdecl; external gtklib;

{ set Notebook, NotebookTab style }
procedure gtk_notebook_set_show_border(notebook:PGtkNotebook; show_border:gboolean); cdecl; external gtklib;
function gtk_notebook_get_show_border(notebook:PGtkNotebook):gboolean; cdecl; external gtklib;
procedure gtk_notebook_set_show_tabs(notebook:PGtkNotebook; show_tabs:gboolean); cdecl; external gtklib;
function gtk_notebook_get_show_tabs(notebook:PGtkNotebook):gboolean; cdecl; external gtklib;
procedure gtk_notebook_set_tab_pos(notebook:PGtkNotebook; pos:TGtkPositionType); cdecl; external gtklib;
function gtk_notebook_get_tab_pos(notebook:PGtkNotebook):TGtkPositionType; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_notebook_set_homogeneous_tabs(notebook:PGtkNotebook; homogeneous:gboolean); cdecl; external gtklib;
procedure gtk_notebook_set_tab_border(notebook:PGtkNotebook; border_width:guint); cdecl; external gtklib;
procedure gtk_notebook_set_tab_hborder(notebook:PGtkNotebook; tab_hborder:guint); cdecl; external gtklib;
procedure gtk_notebook_set_tab_vborder(notebook:PGtkNotebook; tab_vborder:guint); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

procedure gtk_notebook_set_scrollable(notebook:PGtkNotebook; scrollable:gboolean); cdecl; external gtklib;
function gtk_notebook_get_scrollable(notebook:PGtkNotebook):gboolean; cdecl; external gtklib;
{
                 enable/disable PopupMenu
                                                            }
procedure gtk_notebook_popup_enable(notebook:PGtkNotebook); cdecl; external gtklib;
procedure gtk_notebook_popup_disable(notebook:PGtkNotebook); cdecl; external gtklib;
{
               query/set NotebookPage Properties
                                                            }
function gtk_notebook_get_tab_label(notebook:PGtkNotebook; child:PGtkWidget):PGtkWidget; cdecl; external gtklib;
procedure gtk_notebook_set_tab_label(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget); cdecl; external gtklib;
procedure gtk_notebook_set_tab_label_text(notebook:PGtkNotebook; child:PGtkWidget; tab_text:Pgchar); cdecl; external gtklib;
function gtk_notebook_get_tab_label_text(notebook:PGtkNotebook; child:PGtkWidget):Pgchar; cdecl; external gtklib;
function gtk_notebook_get_menu_label(notebook:PGtkNotebook; child:PGtkWidget):PGtkWidget; cdecl; external gtklib;
procedure gtk_notebook_set_menu_label(notebook:PGtkNotebook; child:PGtkWidget; menu_label:PGtkWidget); cdecl; external gtklib;
procedure gtk_notebook_set_menu_label_text(notebook:PGtkNotebook; child:PGtkWidget; menu_text:Pgchar); cdecl; external gtklib;
function gtk_notebook_get_menu_label_text(notebook:PGtkNotebook; child:PGtkWidget):Pgchar; cdecl; external gtklib;
procedure gtk_notebook_query_tab_label_packing(notebook:PGtkNotebook;child:PGtkWidget;expand:Pgboolean;fill:Pgboolean;pack_type:PGtkPackType); cdecl; external gtklib;
procedure gtk_notebook_set_tab_label_packing(notebook:PGtkNotebook; child:PGtkWidget; expand:gboolean; fill:gboolean; pack_type:TGtkPackType); cdecl; external gtklib;
procedure gtk_notebook_reorder_child(notebook:PGtkNotebook; child:PGtkWidget; position:gint); cdecl; external gtklib;

{$IFDEF HasGTK2_2}
function gtk_notebook_get_n_pages(notebook:PGtkNotebook): integer; cdecl; external gtklib;
{$ENDIF}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_NOTEBOOK : GType;
begin
   GTK_TYPE_NOTEBOOK:=gtk_notebook_get_type;
end;

function GTK_NOTEBOOK(obj: pointer) : PGtkNotebook;
begin
   GTK_NOTEBOOK:=PGtkNotebook(GTK_CHECK_CAST(obj,GTK_TYPE_NOTEBOOK));
end;

function GTK_NOTEBOOK_CLASS(klass: pointer) : PGtkNotebookClass;
begin
   GTK_NOTEBOOK_CLASS:=PGtkNotebookClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_NOTEBOOK));
end;

function GTK_IS_NOTEBOOK(obj: pointer) : boolean;
begin
   GTK_IS_NOTEBOOK:=GTK_CHECK_TYPE(obj,GTK_TYPE_NOTEBOOK);
end;

function GTK_IS_NOTEBOOK_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_NOTEBOOK_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_NOTEBOOK);
end;

function GTK_NOTEBOOK_GET_CLASS(obj: pointer) : PGtkNotebookClass;
begin
   GTK_NOTEBOOK_GET_CLASS:=PGtkNotebookClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_NOTEBOOK));
end;

function show_tabs(var a : TGtkNotebook) : guint;
begin
   show_tabs:=(a.flag0 and bm_TGtkNotebook_show_tabs) shr bp_TGtkNotebook_show_tabs;
end;

procedure set_show_tabs(var a : TGtkNotebook; __show_tabs : guint);
begin
   a.flag0:=a.flag0 or ((__show_tabs shl bp_TGtkNotebook_show_tabs) and bm_TGtkNotebook_show_tabs);
end;

function homogeneous(var a : TGtkNotebook) : guint;
begin
   homogeneous:=(a.flag0 and bm_TGtkNotebook_homogeneous) shr bp_TGtkNotebook_homogeneous;
end;

procedure set_homogeneous(var a : TGtkNotebook; __homogeneous : guint);
begin
   a.flag0:=a.flag0 or ((__homogeneous shl bp_TGtkNotebook_homogeneous) and bm_TGtkNotebook_homogeneous);
end;

function show_border(var a : TGtkNotebook) : guint;
begin
   show_border:=(a.flag0 and bm_TGtkNotebook_show_border) shr bp_TGtkNotebook_show_border;
end;

procedure set_show_border(var a : TGtkNotebook; __show_border : guint);
begin
   a.flag0:=a.flag0 or ((__show_border shl bp_TGtkNotebook_show_border) and bm_TGtkNotebook_show_border);
end;

function tab_pos(var a : TGtkNotebook) : guint;
begin
   tab_pos:=(a.flag0 and bm_TGtkNotebook_tab_pos) shr bp_TGtkNotebook_tab_pos;
end;

procedure set_tab_pos(var a : TGtkNotebook; __tab_pos : guint);
begin
   a.flag0:=a.flag0 or ((__tab_pos shl bp_TGtkNotebook_tab_pos) and bm_TGtkNotebook_tab_pos);
end;

function scrollable(var a : TGtkNotebook) : guint;
begin
   scrollable:=(a.flag0 and bm_TGtkNotebook_scrollable) shr bp_TGtkNotebook_scrollable;
end;

procedure set_scrollable(var a : TGtkNotebook; __scrollable : guint);
begin
   a.flag0:=a.flag0 or ((__scrollable shl bp_TGtkNotebook_scrollable) and bm_TGtkNotebook_scrollable);
end;

function in_child(var a : TGtkNotebook) : guint;
begin
   in_child:=(a.flag0 and bm_TGtkNotebook_in_child) shr bp_TGtkNotebook_in_child;
end;

procedure set_in_child(var a : TGtkNotebook; __in_child : guint);
begin
   a.flag0:=a.flag0 or ((__in_child shl bp_TGtkNotebook_in_child) and bm_TGtkNotebook_in_child);
end;

function click_child(var a : TGtkNotebook) : guint;
begin
   click_child:=(a.flag0 and bm_TGtkNotebook_click_child) shr bp_TGtkNotebook_click_child;
end;

procedure set_click_child(var a : TGtkNotebook; __click_child : guint);
begin
   a.flag0:=a.flag0 or ((__click_child shl bp_TGtkNotebook_click_child) and bm_TGtkNotebook_click_child);
end;

function button(var a : TGtkNotebook) : guint;
begin
   button:=(a.flag0 and bm_TGtkNotebook_button) shr bp_TGtkNotebook_button;
end;

procedure set_button(var a : TGtkNotebook; __button : guint);
begin
   a.flag0:=a.flag0 or ((__button shl bp_TGtkNotebook_button) and bm_TGtkNotebook_button);
end;

function need_timer(var a : TGtkNotebook) : guint;
begin
   need_timer:=(a.flag0 and bm_TGtkNotebook_need_timer) shr bp_TGtkNotebook_need_timer;
end;

procedure set_need_timer(var a : TGtkNotebook; __need_timer : guint);
begin
   a.flag0:=a.flag0 or ((__need_timer shl bp_TGtkNotebook_need_timer) and bm_TGtkNotebook_need_timer);
end;

function child_has_focus(var a : TGtkNotebook) : guint;
begin
   child_has_focus:=(a.flag0 and bm_TGtkNotebook_child_has_focus) shr bp_TGtkNotebook_child_has_focus;
end;

procedure set_child_has_focus(var a : TGtkNotebook; __child_has_focus : guint);
begin
   a.flag0:=a.flag0 or ((__child_has_focus shl bp_TGtkNotebook_child_has_focus) and bm_TGtkNotebook_child_has_focus);
end;

function have_visible_child(var a : TGtkNotebook) : guint;
begin
   have_visible_child:=(a.flag0 and bm_TGtkNotebook_have_visible_child) shr bp_TGtkNotebook_have_visible_child;
end;

procedure set_have_visible_child(var a : TGtkNotebook; __have_visible_child : guint);
begin
   a.flag0:=a.flag0 or ((__have_visible_child shl bp_TGtkNotebook_have_visible_child) and bm_TGtkNotebook_have_visible_child);
end;

function focus_out(var a : TGtkNotebook) : guint;
begin
   focus_out:=(a.flag0 and bm_TGtkNotebook_focus_out) shr bp_TGtkNotebook_focus_out;
end;

procedure set_focus_out(var a : TGtkNotebook; __focus_out : guint);
begin
   a.flag0:=a.flag0 or ((__focus_out shl bp_TGtkNotebook_focus_out) and bm_TGtkNotebook_focus_out);
end;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_notebook_current_page(notebook:PGtkNotebook):gint;
begin
   gtk_notebook_current_page:=gtk_notebook_get_current_page(notebook);
end;

procedure gtk_notebook_set_page(notebook:PGtkNotebook; page_num:gint);
begin
   gtk_notebook_set_current_page(notebook,page_num);
end;
{$endif}

{$ENDIF read_implementation}
// included by gtk2.pas

