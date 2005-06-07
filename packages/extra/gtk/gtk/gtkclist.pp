{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}


    type
       TGtkCellType = (GTK_CELL_EMPTY,GTK_CELL_TEXT,GTK_CELL_PIXMAP,
         GTK_CELL_PIXTEXT,GTK_CELL_WIDGET);

       TGtkCListDragPos = (GTK_CLIST_DRAG_NONE,GTK_CLIST_DRAG_BEFORE,
         GTK_CLIST_DRAG_INTO,GTK_CLIST_DRAG_AFTER
         );

       TGtkButtonAction = (GTK_BUTTON_IGNORED := 0,GTK_BUTTON_SELECTS := 1 shl 0,
         GTK_BUTTON_DRAGS := 1 shl 1,GTK_BUTTON_EXPANDS := 1 shl 2
         );

       PGtkCList = ^TGtkCList;
       PGtkCListClass = ^TGtkCListClass;
       PGtkCListColumn = ^TGtkCListColumn;
       PGtkCListRow = ^TGtkCListRow;
       PGtkCellText = ^TGtkCellText;
       PGtkCellPixmap = ^TGtkCellPixmap;
       PGtkCellPixText = ^TGtkCellPixText;
       PGtkCellWidget = ^TGtkCellWidget;
       PGtkCell = ^TGtkCell;

       TGtkCListCompareFunc = function (clist:PGtkCList; ptr1:gconstpointer; ptr2:gconstpointer):gint;cdecl;

       PGtkCListCellInfo = ^TGtkCListCellInfo;
       TGtkCListCellInfo = record
            row : gint;
            column : gint;
         end;

       PGtkCListDestInfo = ^TGtkCListDestInfo;
       TGtkCListDestInfo = record
            cell : TGtkCListCellInfo;
            insert_pos : TGtkCListDragPos;
         end;

       TGtkCList = record
            container : TGtkContainer;
            flags : guint16;
            row_mem_chunk : PGMemChunk;
            cell_mem_chunk : PGMemChunk;
            freeze_count : guint;
            internal_allocation : TGdkRectangle;
            rows : gint;
            row_center_offset : gint;
            row_height : gint;
            row_list : PGList;
            row_list_end : PGList;
            columns : gint;
            column_title_area : TGdkRectangle;
            title_window : PGdkWindow;
            column : PGtkCListColumn;
            clist_window : PGdkWindow;
            clist_window_width : gint;
            clist_window_height : gint;
            hoffset : gint;
            voffset : gint;
            shadow_type : TGtkShadowType;
            selection_mode : TGtkSelectionMode;
            selection : PGList;
            selection_end : PGList;
            undo_selection : PGList;
            undo_unselection : PGList;
            undo_anchor : gint;
            button_actions : array[0..4] of guint8;
            drag_button : guint8;
            click_cell : TGtkCListCellInfo;
            hadjustment : PGtkAdjustment;
            vadjustment : PGtkAdjustment;
            xor_gc : PGdkGC;
            fg_gc : PGdkGC;
            bg_gc : PGdkGC;
            cursor_drag : PGdkCursor;
            x_drag : gint;
            focus_row : gint;
            anchor : gint;
            anchor_state : TGtkStateType;
            drag_pos : gint;
            htimer : gint;
            vtimer : gint;
            sort_type : TGtkSortType;
            compare : TGtkCListCompareFunc;
            sort_column : gint;
         end;

       TGtkCListClass = record
            parent_class : TGtkContainerClass;
            set_scroll_adjustments : procedure (clist:PGtkCList; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment);cdecl;
            refresh : procedure (clist:PGtkCList);cdecl;
            select_row : procedure (clist:PGtkCList; row:gint; column:gint; event:PGdkEvent);cdecl;
            unselect_row : procedure (clist:PGtkCList; row:gint; column:gint; event:PGdkEvent);cdecl;
            row_move : procedure (clist:PGtkCList; source_row:gint; dest_row:gint);cdecl;
            click_column : procedure (clist:PGtkCList; column:gint);cdecl;
            resize_column : procedure (clist:PGtkCList; column:gint; width:gint);cdecl;
            toggle_focus_row : procedure (clist:PGtkCList);cdecl;
            select_all : procedure (clist:PGtkCList);cdecl;
            unselect_all : procedure (clist:PGtkCList);cdecl;
            undo_selection : procedure (clist:PGtkCList);cdecl;
            start_selection : procedure (clist:PGtkCList);cdecl;
            end_selection : procedure (clist:PGtkCList);cdecl;
            extend_selection : procedure (clist:PGtkCList; scroll_type:TGtkScrollType; position:gfloat; auto_start_selection:gboolean);cdecl;
            scroll_horizontal : procedure (clist:PGtkCList; scroll_type:TGtkScrollType; position:gfloat);cdecl;
            scroll_vertical : procedure (clist:PGtkCList; scroll_type:TGtkScrollType; position:gfloat);cdecl;
            toggle_add_mode : procedure (clist:PGtkCList);cdecl;
            abort_column_resize : procedure (clist:PGtkCList);cdecl;
            resync_selection : procedure (clist:PGtkCList; event:PGdkEvent);cdecl;
            selection_find : function (clist:PGtkCList; row_number:gint; row_list_element:PGList):PGList;cdecl;
            draw_row : procedure (clist:PGtkCList; area:PGdkRectangle; row:gint; clist_row:PGtkCListRow);cdecl;
            draw_drag_highlight : procedure (clist:PGtkCList; target_row:PGtkCListRow; target_row_number:gint; drag_pos:TGtkCListDragPos);cdecl;
            clear : procedure (clist:PGtkCList);cdecl;
            fake_unselect_all : procedure (clist:PGtkCList; row:gint);cdecl;
            sort_list : procedure (clist:PGtkCList);cdecl;
            insert_row : procedure (clist:PGtkCList; row:gint; text:PPgchar);cdecl;
            remove_row : procedure (clist:PGtkCList; row:gint);cdecl;
            set_cell_contents : procedure (clist:PGtkCList; clist_row:PGtkCListRow; column:gint; thetype:TGtkCellType; text:Pgchar; spacing:guint8; pixmap:PGdkPixmap; mask:PGdkBitmap);cdecl;
            cell_size_request : procedure (clist:PGtkCList; clist_row:PGtkCListRow; column:gint; requisition:PGtkRequisition);cdecl;
         end;

       TGtkCListColumn = record
            title : Pgchar;
            area : TGdkRectangle;
            button : PGtkWidget;
            window : PGdkWindow;
            width : gint;
            min_width : gint;
            max_width : gint;
            justification : TGtkJustification;
            flag0 : {$ifdef win32}longint{$else}word{$endif};
         end;

       TGtkCListRow = record
            cell : PGtkCell;
            state : TGtkStateType;
            foreground : TGdkColor;
            background : TGdkColor;
            style : PGtkStyle;
            data : gpointer;
            destroy : TGtkDestroyNotify;
            flag0 : {$ifdef win32}longint{$else}word{$endif};
         end;
       TGtkCellText = record
            thetype : TGtkCellType;
            vertical : gint16;
            horizontal : gint16;
            style : PGtkStyle;
            thetext : Pgchar;
         end;
       TGtkCellPixmap = record
            thetype : TGtkCellType;
            vertical : gint16;
            horizontal : gint16;
            style : PGtkStyle;
            pixmap : PGdkPixmap;
            mask : PGdkBitmap;
         end;
       TGtkCellPixText = record
            thetype : TGtkCellType;
            vertical : gint16;
            horizontal : gint16;
            style : PGtkStyle;
            thetext : Pgchar;
            spacing : guint8;
            pixmap : PGdkPixmap;
            mask : PGdkBitmap;
         end;
       TGtkCellWidget = record
            thetype : TGtkCellType;
            vertical : gint16;
            horizontal : gint16;
            style : PGtkStyle;
            widget : PGtkWidget;
         end;
       TGtkCell = record
            thetype : TGtkCellType;
            vertical : gint16;
            horizontal : gint16;
            style : PGtkStyle;
            u : record
                case longint of
                   0 : ( text : Pgchar );
                   1 : ( pm : record
                        pixmap : PGdkPixmap;
                        mask : PGdkBitmap;
                     end );
                   2 : ( pt : record
                        text : Pgchar;
                        spacing : guint8;
                        pixmap : PGdkPixmap;
                        mask : PGdkBitmap;
                     end );
                   3 : ( widget : PGtkWidget );
                end;
         end;

    const
       bm_TGtkCListColumn_visible = $1;
       bp_TGtkCListColumn_visible = 0;
       bm_TGtkCListColumn_width_set = $2;
       bp_TGtkCListColumn_width_set = 1;
       bm_TGtkCListColumn_resizeable = $4;
       bp_TGtkCListColumn_resizeable = 2;
       bm_TGtkCListColumn_auto_resize = $8;
       bp_TGtkCListColumn_auto_resize = 3;
       bm_TGtkCListColumn_button_passive = $10;
       bp_TGtkCListColumn_button_passive = 4;
function  visible(var a : TGtkCListColumn) : guint;
procedure set_visible(var a : TGtkCListColumn; __visible : guint);
function  width_set(var a : TGtkCListColumn) : guint;
procedure set_width_set(var a : TGtkCListColumn; __width_set : guint);
function  resizeable(var a : TGtkCListColumn) : guint;
procedure set_resizeable(var a : TGtkCListColumn; __resizeable : guint);
function  auto_resize(var a : TGtkCListColumn) : guint;
procedure set_auto_resize(var a : TGtkCListColumn; __auto_resize : guint);
function  button_passive(var a : TGtkCListColumn) : guint;
procedure set_button_passive(var a : TGtkCListColumn; __button_passive : guint);
    const
       bm_TGtkCListRow_fg_set = $1;
       bp_TGtkCListRow_fg_set = 0;
       bm_TGtkCListRow_bg_set = $2;
       bp_TGtkCListRow_bg_set = 1;
       bm_TGtkCListRow_selectable = $4;
       bp_TGtkCListRow_selectable = 2;
function  fg_set(var a : TGtkCListRow) : guint;cdecl;
procedure set_fg_set(var a : TGtkCListRow; __fg_set : guint);cdecl;
function  bg_set(var a : TGtkCListRow) : guint;cdecl;
procedure set_bg_set(var a : TGtkCListRow; __bg_set : guint);cdecl;
function  selectable(var a : TGtkCListRow) : guint;
procedure set_selectable(var a : TGtkCListRow; __selectable : guint);

Type
  GTK_CLIST=PGtkCList;
  GTK_CLIST_CLASS=PGtkCListClass;

function  GTK_CLIST_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_clist_get_type';
function  GTK_IS_CLIST(obj:pointer):boolean;
function  GTK_IS_CLIST_CLASS(klass:pointer):boolean;

function  gtk_clist_get_type:TGtkType;cdecl;external gtkdll name 'gtk_clist_get_type';
procedure gtk_clist_construct(clist:PGtkCList; columns:gint; titles:PPgchar);cdecl;external gtkdll name 'gtk_clist_construct';
function  gtk_clist_new (columns:gint):PGtkWidget;cdecl;external gtkdll name 'gtk_clist_new';
function  gtk_clist_new_with_titles (columns:gint; titles:PPgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_clist_new_with_titles';
procedure gtk_clist_set_hadjustment(clist:PGtkCList; adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_clist_set_hadjustment';
procedure gtk_clist_set_vadjustment(clist:PGtkCList; adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_clist_set_vadjustment';
function  gtk_clist_get_hadjustment(clist:PGtkCList):PGtkAdjustment;cdecl;external gtkdll name 'gtk_clist_get_hadjustment';
function  gtk_clist_get_vadjustment(clist:PGtkCList):PGtkAdjustment;cdecl;external gtkdll name 'gtk_clist_get_vadjustment';
procedure gtk_clist_set_shadow_type(clist:PGtkCList; thetype:TGtkShadowType);cdecl;external gtkdll name 'gtk_clist_set_shadow_type';
procedure gtk_clist_set_selection_mode(clist:PGtkCList; mode:TGtkSelectionMode);cdecl;external gtkdll name 'gtk_clist_set_selection_mode';
procedure gtk_clist_set_reorderable(clist:PGtkCList; reorderable:gboolean);cdecl;external gtkdll name 'gtk_clist_set_reorderable';
procedure gtk_clist_set_use_drag_icons(clist:PGtkCList; use_icons:gboolean);cdecl;external gtkdll name 'gtk_clist_set_use_drag_icons';
procedure gtk_clist_set_button_actions(clist:PGtkCList; button:guint; button_actions:guint8);cdecl;external gtkdll name 'gtk_clist_set_button_actions';
procedure gtk_clist_freeze(clist:PGtkCList);cdecl;external gtkdll name 'gtk_clist_freeze';
procedure gtk_clist_thaw(clist:PGtkCList);cdecl;external gtkdll name 'gtk_clist_thaw';
procedure gtk_clist_column_titles_show(clist:PGtkCList);cdecl;external gtkdll name 'gtk_clist_column_titles_show';
procedure gtk_clist_column_titles_hide(clist:PGtkCList);cdecl;external gtkdll name 'gtk_clist_column_titles_hide';
procedure gtk_clist_column_title_active(clist:PGtkCList; column:gint);cdecl;external gtkdll name 'gtk_clist_column_title_active';
procedure gtk_clist_column_title_passive(clist:PGtkCList; column:gint);cdecl;external gtkdll name 'gtk_clist_column_title_passive';
procedure gtk_clist_column_titles_active(clist:PGtkCList);cdecl;external gtkdll name 'gtk_clist_column_titles_active';
procedure gtk_clist_column_titles_passive(clist:PGtkCList);cdecl;external gtkdll name 'gtk_clist_column_titles_passive';
procedure gtk_clist_set_column_title(clist:PGtkCList; column:gint; title:Pgchar);cdecl;external gtkdll name 'gtk_clist_set_column_title';
function  gtk_clist_get_column_title(clist:PGtkCList; column:gint):Pgchar;cdecl;external gtkdll name 'gtk_clist_get_column_title';
procedure gtk_clist_set_column_widget(clist:PGtkCList; column:gint; widget:PGtkWidget);cdecl;external gtkdll name 'gtk_clist_set_column_widget';
function  gtk_clist_get_column_widget(clist:PGtkCList; column:gint):PGtkWidget;cdecl;external gtkdll name 'gtk_clist_get_column_widget';
procedure gtk_clist_set_column_justification(clist:PGtkCList; column:gint; justification:TGtkJustification);cdecl;external gtkdll name 'gtk_clist_set_column_justification';
procedure gtk_clist_set_column_visibility(clist:PGtkCList; column:gint; visible:gboolean);cdecl;external gtkdll name 'gtk_clist_set_column_visibility';
procedure gtk_clist_set_column_resizeable(clist:PGtkCList; column:gint; resizeable:gboolean);cdecl;external gtkdll name 'gtk_clist_set_column_resizeable';
procedure gtk_clist_set_column_auto_resize(clist:PGtkCList; column:gint; auto_resize:gboolean);cdecl;external gtkdll name 'gtk_clist_set_column_auto_resize';
function  gtk_clist_columns_autosize(clist:PGtkCList):gint;cdecl;external gtkdll name 'gtk_clist_columns_autosize';
function  gtk_clist_optimal_column_width(clist:PGtkCList; column:gint):gint;cdecl;external gtkdll name 'gtk_clist_optimal_column_width';
procedure gtk_clist_set_column_width(clist:PGtkCList; column:gint; width:gint);cdecl;external gtkdll name 'gtk_clist_set_column_width';
procedure gtk_clist_set_column_min_width(clist:PGtkCList; column:gint; min_width:gint);cdecl;external gtkdll name 'gtk_clist_set_column_min_width';
procedure gtk_clist_set_column_max_width(clist:PGtkCList; column:gint; max_width:gint);cdecl;external gtkdll name 'gtk_clist_set_column_max_width';
procedure gtk_clist_set_row_height(clist:PGtkCList; height:guint);cdecl;external gtkdll name 'gtk_clist_set_row_height';
procedure gtk_clist_moveto(clist:PGtkCList; row:gint; column:gint; row_align:gfloat; col_align:gfloat);cdecl;external gtkdll name 'gtk_clist_moveto';
function  gtk_clist_row_is_visible(clist:PGtkCList; row:gint):TGtkVisibility;cdecl;external gtkdll name 'gtk_clist_row_is_visible';
function  gtk_clist_get_cell_type(clist:PGtkCList; row:gint; column:gint):TGtkCellType;cdecl;external gtkdll name 'gtk_clist_get_cell_type';
procedure gtk_clist_set_text(clist:PGtkCList; row:gint; column:gint; thetext:Pgchar);cdecl;external gtkdll name 'gtk_clist_set_text';
function  gtk_clist_get_text(clist:PGtkCList; row:gint; column:gint; thetext:PPgchar):gint;cdecl;external gtkdll name 'gtk_clist_get_text';
procedure gtk_clist_set_pixmap(clist:PGtkCList; row:gint; column:gint; pixmap:PGdkPixmap; mask:PGdkBitmap);cdecl;external gtkdll name 'gtk_clist_set_pixmap';
function  gtk_clist_get_pixmap(clist:PGtkCList; row:gint; column:gint; pixmap:PPGdkPixmap; mask:PPGdkBitmap):gint;cdecl;external gtkdll name 'gtk_clist_get_pixmap';
procedure gtk_clist_set_pixtext(clist:PGtkCList; row:gint; column:gint; thetext:Pgchar; spacing:guint8;pixmap:PGdkPixmap; mask:PGdkBitmap);cdecl;external gtkdll name 'gtk_clist_set_pixtext';
function  gtk_clist_get_pixtext(clist:PGtkCList; row:gint; column:gint; thetext:PPgchar; spacing:Pguint8;pixmap:PPGdkPixmap; mask:PPGdkBitmap):gint;cdecl;external gtkdll name 'gtk_clist_get_pixtext';
procedure gtk_clist_set_foreground(clist:PGtkCList; row:gint; color:PGdkColor);cdecl;external gtkdll name 'gtk_clist_set_foreground';
procedure gtk_clist_set_background(clist:PGtkCList; row:gint; color:PGdkColor);cdecl;external gtkdll name 'gtk_clist_set_background';
procedure gtk_clist_set_cell_style(clist:PGtkCList; row:gint; column:gint; style:PGtkStyle);cdecl;external gtkdll name 'gtk_clist_set_cell_style';
function  gtk_clist_get_cell_style(clist:PGtkCList; row:gint; column:gint):PGtkStyle;cdecl;external gtkdll name 'gtk_clist_get_cell_style';
procedure gtk_clist_set_row_style(clist:PGtkCList; row:gint; style:PGtkStyle);cdecl;external gtkdll name 'gtk_clist_set_row_style';
function  gtk_clist_get_row_style(clist:PGtkCList; row:gint):PGtkStyle;cdecl;external gtkdll name 'gtk_clist_get_row_style';
procedure gtk_clist_set_shift(clist:PGtkCList; row:gint; column:gint; vertical:gint; horizontal:gint);cdecl;external gtkdll name 'gtk_clist_set_shift';
function  gtk_clist_prepend(clist:PGtkCList; thetext:PPgchar):gint;cdecl;external gtkdll name 'gtk_clist_prepend';
function  gtk_clist_append(clist:PGtkCList; thetext:PPgchar):gint;cdecl;external gtkdll name 'gtk_clist_append';
procedure gtk_clist_insert(clist:PGtkCList; row:gint; thetext:PPgchar);cdecl;external gtkdll name 'gtk_clist_insert';
procedure gtk_clist_remove(clist:PGtkCList; row:gint);cdecl;external gtkdll name 'gtk_clist_remove';
procedure gtk_clist_set_row_data(clist:PGtkCList; row:gint; data:gpointer);cdecl;external gtkdll name 'gtk_clist_set_row_data';
procedure gtk_clist_set_row_data_full(clist:PGtkCList; row:gint; data:gpointer; destroy:TGtkDestroyNotify);cdecl;external gtkdll name 'gtk_clist_set_row_data_full';
function  gtk_clist_get_row_data(clist:PGtkCList; row:gint):gpointer;cdecl;external gtkdll name 'gtk_clist_get_row_data';
function  gtk_clist_find_row_from_data(clist:PGtkCList; data:gpointer):gint;cdecl;external gtkdll name 'gtk_clist_find_row_from_data';
procedure gtk_clist_select_row(clist:PGtkCList; row:gint; column:gint);cdecl;external gtkdll name 'gtk_clist_select_row';
procedure gtk_clist_unselect_row(clist:PGtkCList; row:gint; column:gint);cdecl;external gtkdll name 'gtk_clist_unselect_row';
procedure gtk_clist_clear(clist:PGtkCList);cdecl;external gtkdll name 'gtk_clist_clear';
function  gtk_clist_get_selection_info(clist:PGtkCList; x:gint; y:gint; row:Pgint; column:Pgint):gint;cdecl;external gtkdll name 'gtk_clist_get_selection_info';
procedure gtk_clist_select_all(clist:PGtkCList);cdecl;external gtkdll name 'gtk_clist_select_all';
procedure gtk_clist_unselect_all(clist:PGtkCList);cdecl;external gtkdll name 'gtk_clist_unselect_all';
procedure gtk_clist_swap_rows(clist:PGtkCList; row1:gint; row2:gint);cdecl;external gtkdll name 'gtk_clist_swap_rows';
procedure gtk_clist_row_move(clist:PGtkCList; source_row:gint; dest_row:gint);cdecl;external gtkdll name 'gtk_clist_row_move';
procedure gtk_clist_set_compare_func(clist:PGtkCList; cmp_func:TGtkCListCompareFunc);cdecl;external gtkdll name 'gtk_clist_set_compare_func';
procedure gtk_clist_set_sort_column(clist:PGtkCList; column:gint);cdecl;external gtkdll name 'gtk_clist_set_sort_column';
procedure gtk_clist_set_sort_type(clist:PGtkCList; sort_type:TGtkSortType);cdecl;external gtkdll name 'gtk_clist_set_sort_type';
procedure gtk_clist_sort(clist:PGtkCList);cdecl;external gtkdll name 'gtk_clist_sort';
procedure gtk_clist_set_auto_sort(clist:PGtkCList; auto_sort:gboolean);cdecl;external gtkdll name 'gtk_clist_set_auto_sort';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  visible(var a : TGtkCListColumn) : guint;
      begin
         visible:=(a.flag0 and bm_TGtkCListColumn_visible) shr bp_TGtkCListColumn_visible;
      end;

procedure set_visible(var a : TGtkCListColumn; __visible : guint);
      begin
         a.flag0:=a.flag0 or ((__visible shl bp_TGtkCListColumn_visible) and bm_TGtkCListColumn_visible);
      end;

function  width_set(var a : TGtkCListColumn) : guint;
      begin
         width_set:=(a.flag0 and bm_TGtkCListColumn_width_set) shr bp_TGtkCListColumn_width_set;
      end;

procedure set_width_set(var a : TGtkCListColumn; __width_set : guint);
      begin
         a.flag0:=a.flag0 or ((__width_set shl bp_TGtkCListColumn_width_set) and bm_TGtkCListColumn_width_set);
      end;

function  resizeable(var a : TGtkCListColumn) : guint;
      begin
         resizeable:=(a.flag0 and bm_TGtkCListColumn_resizeable) shr bp_TGtkCListColumn_resizeable;
      end;

procedure set_resizeable(var a : TGtkCListColumn; __resizeable : guint);
      begin
         a.flag0:=a.flag0 or ((__resizeable shl bp_TGtkCListColumn_resizeable) and bm_TGtkCListColumn_resizeable);
      end;

function  auto_resize(var a : TGtkCListColumn) : guint;
      begin
         auto_resize:=(a.flag0 and bm_TGtkCListColumn_auto_resize) shr bp_TGtkCListColumn_auto_resize;
      end;

procedure set_auto_resize(var a : TGtkCListColumn; __auto_resize : guint);
      begin
         a.flag0:=a.flag0 or ((__auto_resize shl bp_TGtkCListColumn_auto_resize) and bm_TGtkCListColumn_auto_resize);
      end;

function  button_passive(var a : TGtkCListColumn) : guint;
      begin
         button_passive:=(a.flag0 and bm_TGtkCListColumn_button_passive) shr bp_TGtkCListColumn_button_passive;
      end;

procedure set_button_passive(var a : TGtkCListColumn; __button_passive : guint);
      begin
         a.flag0:=a.flag0 or ((__button_passive shl bp_TGtkCListColumn_button_passive) and bm_TGtkCListColumn_button_passive);
      end;

function  fg_set(var a : TGtkCListRow) : guint;cdecl;
      begin
         fg_set:=(a.flag0 and bm_TGtkCListRow_fg_set) shr bp_TGtkCListRow_fg_set;
      end;

procedure set_fg_set(var a : TGtkCListRow; __fg_set : guint);cdecl;
      begin
         a.flag0:=a.flag0 or ((__fg_set shl bp_TGtkCListRow_fg_set) and bm_TGtkCListRow_fg_set);
      end;

function  bg_set(var a : TGtkCListRow) : guint;cdecl;
      begin
         bg_set:=(a.flag0 and bm_TGtkCListRow_bg_set) shr bp_TGtkCListRow_bg_set;
      end;

procedure set_bg_set(var a : TGtkCListRow; __bg_set : guint);cdecl;
      begin
         a.flag0:=a.flag0 or ((__bg_set shl bp_TGtkCListRow_bg_set) and bm_TGtkCListRow_bg_set);
      end;

function  selectable(var a : TGtkCListRow) : guint;
      begin
         selectable:=(a.flag0 and bm_TGtkCListRow_selectable) shr bp_TGtkCListRow_selectable;
      end;

procedure set_selectable(var a : TGtkCListRow; __selectable : guint);
      begin
         a.flag0:=a.flag0 or ((__selectable shl bp_TGtkCListRow_selectable) and bm_TGtkCListRow_selectable);
      end;

function  GTK_IS_CLIST(obj:pointer):boolean;
begin
  GTK_IS_CLIST:=(obj<>nil) and GTK_IS_CLIST_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_CLIST_CLASS(klass:pointer):boolean;
begin
  GTK_IS_CLIST_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_CLIST_TYPE);
end;

{$endif read_implementation}


