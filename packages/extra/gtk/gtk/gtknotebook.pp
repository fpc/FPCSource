{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkNotebook = ^TGtkNotebook;
     PGtkNotebookClass = ^TGtkNotebookClass;
     PGtkNotebookPage = ^TGtkNotebookPage;

     TGtkNotebook = record
          container : TGtkContainer;
          cur_page : PGtkNotebookPage;
          children : PGList;
          first_tab : PGList;
          focus_tab : PGList;
          menu : PGtkWidget;
          panel : PGdkWindow;
          timer : guint32;
          tab_hborder : guint16;
          tab_vborder : guint16;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
       end;

     TGtkNotebookClass = record
          parent_class : TGtkContainerClass;
          switch_page : procedure (notebook:PGtkNotebook; page:PGtkNotebookPage; page_num:guint);cdecl;
       end;

     TGtkNotebookPage = record
          child : PGtkWidget;
          tab_label : PGtkWidget;
          menu_label : PGtkWidget;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          requisition : TGtkRequisition;
          allocation : TGtkAllocation;
          pad1 : gint16;
       end;

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
function  show_tabs(var a : TGtkNotebook) : guint;
procedure set_show_tabs(var a : TGtkNotebook; __show_tabs : guint);
function  homogeneous(var a : TGtkNotebook) : guint;
procedure set_homogeneous(var a : TGtkNotebook; __homogeneous : guint);
function  show_border(var a : TGtkNotebook) : guint;
procedure set_show_border(var a : TGtkNotebook; __show_border : guint);
function  tab_pos(var a : TGtkNotebook) : guint;
procedure set_tab_pos(var a : TGtkNotebook; __tab_pos : guint);
function  scrollable(var a : TGtkNotebook) : guint;
procedure set_scrollable(var a : TGtkNotebook; __scrollable : guint);
function  in_child(var a : TGtkNotebook) : guint;
procedure set_in_child(var a : TGtkNotebook; __in_child : guint);
function  click_child(var a : TGtkNotebook) : guint;
procedure set_click_child(var a : TGtkNotebook; __click_child : guint);
function  button(var a : TGtkNotebook) : guint;
procedure set_button(var a : TGtkNotebook; __button : guint);
function  need_timer(var a : TGtkNotebook) : guint;
procedure set_need_timer(var a : TGtkNotebook; __need_timer : guint);
function  child_has_focus(var a : TGtkNotebook) : guint;
procedure set_child_has_focus(var a : TGtkNotebook; __child_has_focus : guint);
function  have_visible_child(var a : TGtkNotebook) : guint;
procedure set_have_visible_child(var a : TGtkNotebook; __have_visible_child : guint);

  const
     bm_TGtkNotebookPage_default_menu = $1;
     bp_TGtkNotebookPage_default_menu = 0;
     bm_TGtkNotebookPage_default_tab = $2;
     bp_TGtkNotebookPage_default_tab = 1;
     bm_TGtkNotebookPage_expand = $4;
     bp_TGtkNotebookPage_expand = 2;
     bm_TGtkNotebookPage_fill = $8;
     bp_TGtkNotebookPage_fill = 3;
     bm_TGtkNotebookPage_pack = $10;
     bp_TGtkNotebookPage_pack = 4;
function  default_menu(var a : TGtkNotebookPage) : guint;
procedure set_default_menu(var a : TGtkNotebookPage; __default_menu : guint);
function  default_tab(var a : TGtkNotebookPage) : guint;
procedure set_default_tab(var a : TGtkNotebookPage; __default_tab : guint);
function  expand(var a : TGtkNotebookPage) : guint;
procedure set_expand(var a : TGtkNotebookPage; __expand : guint);
function  fill(var a : TGtkNotebookPage) : guint;
procedure set_fill(var a : TGtkNotebookPage; __fill : guint);
function  pack(var a : TGtkNotebookPage) : guint;
procedure set_pack(var a : TGtkNotebookPage; __pack : guint);

Type
  GTK_NOTEBOOK=PGtkNotebook;
  GTK_NOTEBOOK_CLASS=PGtkNotebookClass;

function  GTK_NOTEBOOK_PAGE(_glist_ : PGList) : PGtkNotebookPage;

function  GTK_NOTEBOOK_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_notebook_get_type';
function  GTK_IS_NOTEBOOK(obj:pointer):boolean;
function  GTK_IS_NOTEBOOK_CLASS(klass:pointer):boolean;

function  gtk_notebook_get_type:TGtkType;cdecl;external gtkdll name 'gtk_notebook_get_type';
function  gtk_notebook_new:PGtkWidget;cdecl;external gtkdll name 'gtk_notebook_new';
procedure gtk_notebook_append_page(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget);cdecl;external gtkdll name 'gtk_notebook_append_page';
procedure gtk_notebook_append_page_menu(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget; menu_label:PGtkWidget);cdecl;external gtkdll name 'gtk_notebook_append_page_menu';
procedure gtk_notebook_prepend_page(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget);cdecl;external gtkdll name 'gtk_notebook_prepend_page';
procedure gtk_notebook_prepend_page_menu(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget; menu_label:PGtkWidget);cdecl;external gtkdll name 'gtk_notebook_prepend_page_menu';
procedure gtk_notebook_insert_page(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget; position:gint);cdecl;external gtkdll name 'gtk_notebook_insert_page';
procedure gtk_notebook_insert_page_menu(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget; menu_label:PGtkWidget; position:gint);cdecl;external gtkdll name 'gtk_notebook_insert_page_menu';
procedure gtk_notebook_remove_page(notebook:PGtkNotebook; page_num:gint);cdecl;external gtkdll name 'gtk_notebook_remove_page';
function  gtk_notebook_get_current_page(notebook:PGtkNotebook):gint;cdecl;external gtkdll name 'gtk_notebook_get_current_page';
function  gtk_notebook_get_nth_page(notebook:PGtkNotebook; page_num:gint):PGtkWidget;cdecl;external gtkdll name 'gtk_notebook_get_nth_page';
function  gtk_notebook_page_num(notebook:PGtkNotebook; child:PGtkWidget):gint;cdecl;external gtkdll name 'gtk_notebook_page_num';
procedure gtk_notebook_set_page(notebook:PGtkNotebook; page_num:gint);cdecl;external gtkdll name 'gtk_notebook_set_page';
procedure gtk_notebook_next_page(notebook:PGtkNotebook);cdecl;external gtkdll name 'gtk_notebook_next_page';
procedure gtk_notebook_prev_page(notebook:PGtkNotebook);cdecl;external gtkdll name 'gtk_notebook_prev_page';
procedure gtk_notebook_set_show_border(notebook:PGtkNotebook; show_border:gboolean);cdecl;external gtkdll name 'gtk_notebook_set_show_border';
procedure gtk_notebook_set_show_tabs(notebook:PGtkNotebook; show_tabs:gboolean);cdecl;external gtkdll name 'gtk_notebook_set_show_tabs';
procedure gtk_notebook_set_tab_pos(notebook:PGtkNotebook; pos:TGtkPositionType);cdecl;external gtkdll name 'gtk_notebook_set_tab_pos';
procedure gtk_notebook_set_homogeneous_tabs(notebook:PGtkNotebook; homogeneous:gboolean);cdecl;external gtkdll name 'gtk_notebook_set_homogeneous_tabs';
procedure gtk_notebook_set_tab_border(notebook:PGtkNotebook; border_width:guint);cdecl;external gtkdll name 'gtk_notebook_set_tab_border';
procedure gtk_notebook_set_tab_hborder(notebook:PGtkNotebook; tab_hborder:guint);cdecl;external gtkdll name 'gtk_notebook_set_tab_hborder';
procedure gtk_notebook_set_tab_vborder(notebook:PGtkNotebook; tab_vborder:guint);cdecl;external gtkdll name 'gtk_notebook_set_tab_vborder';
procedure gtk_notebook_set_scrollable(notebook:PGtkNotebook; scrollable:gboolean);cdecl;external gtkdll name 'gtk_notebook_set_scrollable';
procedure gtk_notebook_popup_enable(notebook:PGtkNotebook);cdecl;external gtkdll name 'gtk_notebook_popup_enable';
procedure gtk_notebook_popup_disable(notebook:PGtkNotebook);cdecl;external gtkdll name 'gtk_notebook_popup_disable';
function  gtk_notebook_get_tab_label(notebook:PGtkNotebook; child:PGtkWidget):PGtkWidget;cdecl;external gtkdll name 'gtk_notebook_get_tab_label';
procedure gtk_notebook_set_tab_label(notebook:PGtkNotebook; child:PGtkWidget; tab_label:PGtkWidget);cdecl;external gtkdll name 'gtk_notebook_set_tab_label';
procedure gtk_notebook_set_tab_label_text(notebook:PGtkNotebook; child:PGtkWidget; tab_text:Pgchar);cdecl;external gtkdll name 'gtk_notebook_set_tab_label_text';
function  gtk_notebook_get_menu_label(notebook:PGtkNotebook; child:PGtkWidget):PGtkWidget;cdecl;external gtkdll name 'gtk_notebook_get_menu_label';
procedure gtk_notebook_set_menu_label(notebook:PGtkNotebook; child:PGtkWidget; menu_label:PGtkWidget);cdecl;external gtkdll name 'gtk_notebook_set_menu_label';
procedure gtk_notebook_set_menu_label_text(notebook:PGtkNotebook; child:PGtkWidget; menu_text:Pgchar);cdecl;external gtkdll name 'gtk_notebook_set_menu_label_text';
procedure gtk_notebook_query_tab_label_packing(notebook:PGtkNotebook; child:PGtkWidget; expand:Pgboolean; fill:Pgboolean; pack_type:PGtkPackType);cdecl;external gtkdll name 'gtk_notebook_query_tab_label_packing';
procedure gtk_notebook_set_tab_label_packing(notebook:PGtkNotebook; child:PGtkWidget; expand:gboolean; fill:gboolean; pack_type:TGtkPackType);cdecl;external gtkdll name 'gtk_notebook_set_tab_label_packing';
procedure gtk_notebook_reorder_child(notebook:PGtkNotebook; child:PGtkWidget; position:gint);cdecl;external gtkdll name 'gtk_notebook_reorder_child';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  show_tabs(var a : TGtkNotebook) : guint;
    begin
       show_tabs:=(a.flag0 and bm_TGtkNotebook_show_tabs) shr bp_TGtkNotebook_show_tabs;
    end;

procedure set_show_tabs(var a : TGtkNotebook; __show_tabs : guint);
    begin
       a.flag0:=a.flag0 or ((__show_tabs shl bp_TGtkNotebook_show_tabs) and bm_TGtkNotebook_show_tabs);
    end;

function  homogeneous(var a : TGtkNotebook) : guint;
    begin
       homogeneous:=(a.flag0 and bm_TGtkNotebook_homogeneous) shr bp_TGtkNotebook_homogeneous;
    end;

procedure set_homogeneous(var a : TGtkNotebook; __homogeneous : guint);
    begin
       a.flag0:=a.flag0 or ((__homogeneous shl bp_TGtkNotebook_homogeneous) and bm_TGtkNotebook_homogeneous);
    end;

function  show_border(var a : TGtkNotebook) : guint;
    begin
       show_border:=(a.flag0 and bm_TGtkNotebook_show_border) shr bp_TGtkNotebook_show_border;
    end;

procedure set_show_border(var a : TGtkNotebook; __show_border : guint);
    begin
       a.flag0:=a.flag0 or ((__show_border shl bp_TGtkNotebook_show_border) and bm_TGtkNotebook_show_border);
    end;

function  tab_pos(var a : TGtkNotebook) : guint;
    begin
       tab_pos:=(a.flag0 and bm_TGtkNotebook_tab_pos) shr bp_TGtkNotebook_tab_pos;
    end;

procedure set_tab_pos(var a : TGtkNotebook; __tab_pos : guint);
    begin
       a.flag0:=a.flag0 or ((__tab_pos shl bp_TGtkNotebook_tab_pos) and bm_TGtkNotebook_tab_pos);
    end;

function  scrollable(var a : TGtkNotebook) : guint;
    begin
       scrollable:=(a.flag0 and bm_TGtkNotebook_scrollable) shr bp_TGtkNotebook_scrollable;
    end;

procedure set_scrollable(var a : TGtkNotebook; __scrollable : guint);
    begin
       a.flag0:=a.flag0 or ((__scrollable shl bp_TGtkNotebook_scrollable) and bm_TGtkNotebook_scrollable);
    end;

function  in_child(var a : TGtkNotebook) : guint;
    begin
       in_child:=(a.flag0 and bm_TGtkNotebook_in_child) shr bp_TGtkNotebook_in_child;
    end;

procedure set_in_child(var a : TGtkNotebook; __in_child : guint);
    begin
       a.flag0:=a.flag0 or ((__in_child shl bp_TGtkNotebook_in_child) and bm_TGtkNotebook_in_child);
    end;

function  click_child(var a : TGtkNotebook) : guint;
    begin
       click_child:=(a.flag0 and bm_TGtkNotebook_click_child) shr bp_TGtkNotebook_click_child;
    end;

procedure set_click_child(var a : TGtkNotebook; __click_child : guint);
    begin
       a.flag0:=a.flag0 or ((__click_child shl bp_TGtkNotebook_click_child) and bm_TGtkNotebook_click_child);
    end;

function  button(var a : TGtkNotebook) : guint;
    begin
       button:=(a.flag0 and bm_TGtkNotebook_button) shr bp_TGtkNotebook_button;
    end;

procedure set_button(var a : TGtkNotebook; __button : guint);
    begin
       a.flag0:=a.flag0 or ((__button shl bp_TGtkNotebook_button) and bm_TGtkNotebook_button);
    end;

function  need_timer(var a : TGtkNotebook) : guint;
    begin
       need_timer:=(a.flag0 and bm_TGtkNotebook_need_timer) shr bp_TGtkNotebook_need_timer;
    end;

procedure set_need_timer(var a : TGtkNotebook; __need_timer : guint);
    begin
       a.flag0:=a.flag0 or ((__need_timer shl bp_TGtkNotebook_need_timer) and bm_TGtkNotebook_need_timer);
    end;

function  child_has_focus(var a : TGtkNotebook) : guint;
    begin
       child_has_focus:=(a.flag0 and bm_TGtkNotebook_child_has_focus) shr bp_TGtkNotebook_child_has_focus;
    end;

procedure set_child_has_focus(var a : TGtkNotebook; __child_has_focus : guint);
    begin
       a.flag0:=a.flag0 or ((__child_has_focus shl bp_TGtkNotebook_child_has_focus) and bm_TGtkNotebook_child_has_focus);
    end;

function  have_visible_child(var a : TGtkNotebook) : guint;
    begin
       have_visible_child:=(a.flag0 and bm_TGtkNotebook_have_visible_child) shr bp_TGtkNotebook_have_visible_child;
    end;

procedure set_have_visible_child(var a : TGtkNotebook; __have_visible_child : guint);
    begin
       a.flag0:=a.flag0 or ((__have_visible_child shl bp_TGtkNotebook_have_visible_child) and bm_TGtkNotebook_have_visible_child);
    end;

function  default_menu(var a : TGtkNotebookPage) : guint;
    begin
       default_menu:=(a.flag0 and bm_TGtkNotebookPage_default_menu) shr bp_TGtkNotebookPage_default_menu;
    end;

procedure set_default_menu(var a : TGtkNotebookPage; __default_menu : guint);
    begin
       a.flag0:=a.flag0 or ((__default_menu shl bp_TGtkNotebookPage_default_menu) and bm_TGtkNotebookPage_default_menu);
    end;

function  default_tab(var a : TGtkNotebookPage) : guint;
    begin
       default_tab:=(a.flag0 and bm_TGtkNotebookPage_default_tab) shr bp_TGtkNotebookPage_default_tab;
    end;

procedure set_default_tab(var a : TGtkNotebookPage; __default_tab : guint);
    begin
       a.flag0:=a.flag0 or ((__default_tab shl bp_TGtkNotebookPage_default_tab) and bm_TGtkNotebookPage_default_tab);
    end;

function  expand(var a : TGtkNotebookPage) : guint;
    begin
       expand:=(a.flag0 and bm_TGtkNotebookPage_expand) shr bp_TGtkNotebookPage_expand;
    end;

procedure set_expand(var a : TGtkNotebookPage; __expand : guint);
    begin
       a.flag0:=a.flag0 or ((__expand shl bp_TGtkNotebookPage_expand) and bm_TGtkNotebookPage_expand);
    end;

function  fill(var a : TGtkNotebookPage) : guint;
    begin
       fill:=(a.flag0 and bm_TGtkNotebookPage_fill) shr bp_TGtkNotebookPage_fill;
    end;

procedure set_fill(var a : TGtkNotebookPage; __fill : guint);
    begin
       a.flag0:=a.flag0 or ((__fill shl bp_TGtkNotebookPage_fill) and bm_TGtkNotebookPage_fill);
    end;

function  pack(var a : TGtkNotebookPage) : guint;
    begin
       pack:=(a.flag0 and bm_TGtkNotebookPage_pack) shr bp_TGtkNotebookPage_pack;
    end;

procedure set_pack(var a : TGtkNotebookPage; __pack : guint);
    begin
       a.flag0:=a.flag0 or ((__pack shl bp_TGtkNotebookPage_pack) and bm_TGtkNotebookPage_pack);
    end;

function  GTK_NOTEBOOK_PAGE(_glist_ : PGList) : PGtkNotebookPage;
    begin
       GTK_NOTEBOOK_PAGE:=PGtkNotebookPage(_glist_^.data);
    end;

function  GTK_IS_NOTEBOOK(obj:pointer):boolean;
begin
  GTK_IS_NOTEBOOK:=(obj<>nil) and GTK_IS_NOTEBOOK_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_NOTEBOOK_CLASS(klass:pointer):boolean;
begin
  GTK_IS_NOTEBOOK_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_NOTEBOOK_TYPE);
end;

{$endif read_implementation}


