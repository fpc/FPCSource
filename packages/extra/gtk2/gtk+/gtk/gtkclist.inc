// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkCList = ^TGtkCList;
   PGtkCListColumn = ^TGtkCListColumn;
   PGtkCListRow = ^TGtkCListRow;
   PGtkCell = ^TGtkCell;

{ cell types  }
   PGtkCellType = ^TGtkCellType;
   TGtkCellType = (
     GTK_CELL_EMPTY,
     GTK_CELL_TEXT,
     GTK_CELL_PIXMAP,
     GTK_CELL_PIXTEXT,
     GTK_CELL_WIDGET
   );

   PGtkCListDragPos = ^TGtkCListDragPos;
   TGtkCListDragPos = (
     GTK_CLIST_DRAG_NONE,
     GTK_CLIST_DRAG_BEFORE,
     GTK_CLIST_DRAG_INTO,
     GTK_CLIST_DRAG_AFTER
   );

   PGtkButtonAction = ^TGtkButtonAction;
   TGtkButtonAction = longint;


   TGtkCListCompareFunc = function (clist:PGtkCList; ptr1:gconstpointer; ptr2:gconstpointer):gint; cdecl;
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

{ mem chunks  }
{ allocation rectangle after the conatiner_border_width
     and the width of the shadow border  }
{ rows  }
{ columns  }
{ dynamicly allocated array of column structures  }
{ the scrolling window and its height and width to
     make things a little speedier  }
{ offsets for scrolling  }
{ border shadow style  }
{ the list's selection mode (gtkenums.h)  }
{ list of selected rows  }
{ mouse buttons  }
{ dnd  }
{ scroll adjustments  }
{ xor GC for the vertical drag line  }
{ gc for drawing unselected cells  }
{ cursor used to indicate dragging  }
{ the current x-pixel location of the xor-drag line  }
{ focus handling  }
{ dragging the selection  }
   TGtkCList = record
        container : TGtkContainer;
        flags : guint16;
        row_mem_chunk : PGMemChunk;
        cell_mem_chunk : PGMemChunk;
        freeze_count : guint;
        internal_allocation : TGdkRectangle;
        rows : gint;
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
        focus_header_column : gint;
        anchor : gint;
        anchor_state : TGtkStateType;
        drag_pos : gint;
        htimer : gint;
        vtimer : gint;
        sort_type : TGtkSortType;
        compare : TGtkCListCompareFunc;
        sort_column : gint;
        drag_highlight_row : gint;
        drag_highlight_pos : TGtkCListDragPos;
     end;

   PGtkCListClass = ^TGtkCListClass;
   TGtkCListClass = record
        parent_class : TGtkContainerClass;
        set_scroll_adjustments : procedure (clist:PGtkCList; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment); cdecl;
        refresh : procedure (clist:PGtkCList); cdecl;
        select_row : procedure (clist:PGtkCList; row:gint; column:gint; event:PGdkEvent); cdecl;
        unselect_row : procedure (clist:PGtkCList; row:gint; column:gint; event:PGdkEvent); cdecl;
        row_move : procedure (clist:PGtkCList; source_row:gint; dest_row:gint); cdecl;
        click_column : procedure (clist:PGtkCList; column:gint); cdecl;
        resize_column : procedure (clist:PGtkCList; column:gint; width:gint); cdecl;
        toggle_focus_row : procedure (clist:PGtkCList); cdecl;
        select_all : procedure (clist:PGtkCList); cdecl;
        unselect_all : procedure (clist:PGtkCList); cdecl;
        undo_selection : procedure (clist:PGtkCList); cdecl;
        start_selection : procedure (clist:PGtkCList); cdecl;
        end_selection : procedure (clist:PGtkCList); cdecl;
        extend_selection : procedure (clist:PGtkCList; scroll_type:TGtkScrollType; position:gfloat; auto_start_selection:gboolean); cdecl;
        scroll_horizontal : procedure (clist:PGtkCList; scroll_type:TGtkScrollType; position:gfloat); cdecl;
        scroll_vertical : procedure (clist:PGtkCList; scroll_type:TGtkScrollType; position:gfloat); cdecl;
        toggle_add_mode : procedure (clist:PGtkCList); cdecl;
        abort_column_resize : procedure (clist:PGtkCList); cdecl;
        resync_selection : procedure (clist:PGtkCList; event:PGdkEvent); cdecl;
        selection_find : function (clist:PGtkCList; row_number:gint; row_list_element:PGList):PGList; cdecl;
        draw_row : procedure (clist:PGtkCList; area:PGdkRectangle; row:gint; clist_row:PGtkCListRow); cdecl;
        draw_drag_highlight : procedure (clist:PGtkCList; target_row:PGtkCListRow; target_row_number:gint; drag_pos:TGtkCListDragPos); cdecl;
        clear : procedure (clist:PGtkCList); cdecl;
        fake_unselect_all : procedure (clist:PGtkCList; row:gint); cdecl;
        sort_list : procedure (clist:PGtkCList); cdecl;
        insert_row : function (clist:PGtkCList; row:gint; text:array of Pgchar):gint; cdecl;
        remove_row : procedure (clist:PGtkCList; row:gint); cdecl;
        set_cell_contents : procedure (clist:PGtkCList; clist_row:PGtkCListRow; column:gint; _type:TGtkCellType; text:Pgchar;
                      spacing:guint8; pixmap:PGdkPixmap; mask:PGdkBitmap); cdecl;
        cell_size_request : procedure (clist:PGtkCList; clist_row:PGtkCListRow; column:gint; requisition:PGtkRequisition); cdecl;
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
        flag0 : word;
     end;

   TGtkCListRow = record
        cell : PGtkCell;
        state : TGtkStateType;
        foreground : TGdkColor;
        background : TGdkColor;
        style : PGtkStyle;
        data : gpointer;
        destroy : TGtkDestroyNotify;
        flag0 : word;
     end;

{ Cell Structures  }
   PGtkCellText = ^TGtkCellText;
   TGtkCellText = record
        _type : TGtkCellType;
        vertical : gint16;
        horizontal : gint16;
        style : PGtkStyle;
        text : Pgchar;
     end;

   PGtkCellPixmap = ^TGtkCellPixmap;
   TGtkCellPixmap = record
        _type : TGtkCellType;
        vertical : gint16;
        horizontal : gint16;
        style : PGtkStyle;
        pixmap : PGdkPixmap;
        mask : PGdkBitmap;
     end;

   PGtkCellPixText = ^TGtkCellPixText;
   TGtkCellPixText = record
        _type : TGtkCellType;
        vertical : gint16;
        horizontal : gint16;
        style : PGtkStyle;
        text : Pgchar;
        spacing : guint8;
        pixmap : PGdkPixmap;
        mask : PGdkBitmap;
     end;

   PGtkCellWidget = ^TGtkCellWidget;
   TGtkCellWidget = record
        _type : TGtkCellType;
        vertical : gint16;
        horizontal : gint16;
        style : PGtkStyle;
        widget : PGtkWidget;
     end;

   TGtkCell = record
        _type : TGtkCellType;
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


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
{ clist flags  }
   GTK_CLIST_IN_DRAG = 1 shl 0;
   GTK_CLIST_ROW_HEIGHT_SET = 1 shl 1;
   GTK_CLIST_SHOW_TITLES = 1 shl 2;
   GTK_CLIST_ADD_MODE = 1 shl 4;
   GTK_CLIST_AUTO_SORT = 1 shl 5;
   GTK_CLIST_AUTO_RESIZE_BLOCKED = 1 shl 6;
   GTK_CLIST_REORDERABLE = 1 shl 7;
   GTK_CLIST_USE_DRAG_ICONS = 1 shl 8;
   GTK_CLIST_DRAW_DRAG_LINE = 1 shl 9;
   GTK_CLIST_DRAW_DRAG_RECT = 1 shl 10;

   GTK_BUTTON_IGNORED = 0;
   GTK_BUTTON_SELECTS = 1 shl 0;
   GTK_BUTTON_DRAGS = 1 shl 1;
   GTK_BUTTON_EXPANDS = 1 shl 2;


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

   bm_TGtkCListRow_fg_set = $1;
   bp_TGtkCListRow_fg_set = 0;
   bm_TGtkCListRow_bg_set = $2;
   bp_TGtkCListRow_bg_set = 1;
   bm_TGtkCListRow_selectable = $4;
   bp_TGtkCListRow_selectable = 2;


function GTK_TYPE_CLIST : GType;
function GTK_CLIST(obj: pointer) : PGtkCList;
function GTK_CLIST_CLASS(klass: pointer) : PGtkCListClass;
function GTK_IS_CLIST(obj: pointer) : boolean;
function GTK_IS_CLIST_CLASS(klass: pointer) : boolean;
function GTK_CLIST_GET_CLASS(obj: pointer) : PGtkCListClass;
function GTK_CLIST_FLAGS(clist : pointer) : guint16;

procedure GTK_CLIST_SET_FLAG(clist: PGtkCList; flag: guint16);
//     (GTK_CLIST_FLAGS (clist) |= (GTK_ ## flag))  }
procedure GTK_CLIST_UNSET_FLAG(clist: PGtkCList; flag: guint16);
//   (GTK_CLIST_FLAGS (clist) &= ~(GTK_ ## flag))  }
function GTK_CLIST_IN_DRAG_get(clist : pointer) : boolean;
function GTK_CLIST_ROW_HEIGHT_SET_get(clist : pointer) : boolean;
function GTK_CLIST_SHOW_TITLES_get(clist : pointer) : boolean;
function GTK_CLIST_ADD_MODE_get(clist : pointer) : boolean;
function GTK_CLIST_AUTO_SORT_get(clist : pointer) : boolean;
function GTK_CLIST_AUTO_RESIZE_BLOCKED_get(clist : pointer) : boolean;
function GTK_CLIST_REORDERABLE_get(clist : pointer) : boolean;
function GTK_CLIST_USE_DRAG_ICONS_get(clist : pointer) : boolean;
function GTK_CLIST_DRAW_DRAG_LINE_get(clist : pointer) : boolean;
function GTK_CLIST_DRAW_DRAG_RECT_get(clist : pointer) : boolean;
function GTK_CLIST_ROW_get(_glist_ : PGList) : PGtkCListRow;

{ pointer casting for cells  }
function GTK_CELL_TEXT_get(cell : pointer) : PGtkCellText;
function GTK_CELL_PIXMAP_get(cell : pointer) : PGtkCellPixmap;
function GTK_CELL_PIXTEXT_get(cell : pointer) : PGtkCellPixText;
function GTK_CELL_WIDGET_get(cell : pointer) : PGtkCellWidget;


function visible(var a : TGtkCListColumn) : guint; overload;
procedure set_visible(var a : TGtkCListColumn; __visible : guint); overload;
function width_set(var a : TGtkCListColumn) : guint;
procedure set_width_set(var a : TGtkCListColumn; __width_set : guint);
function resizeable(var a : TGtkCListColumn) : guint;
procedure set_resizeable(var a : TGtkCListColumn; __resizeable : guint);
function auto_resize(var a : TGtkCListColumn) : guint;
procedure set_auto_resize(var a : TGtkCListColumn; __auto_resize : guint);
function button_passive(var a : TGtkCListColumn) : guint;
procedure set_button_passive(var a : TGtkCListColumn; __button_passive : guint);


function fg_set(var a : TGtkCListRow) : guint;
procedure set_fg_set(var a : TGtkCListRow; __fg_set : guint);
function bg_set(var a : TGtkCListRow) : guint;
procedure set_bg_set(var a : TGtkCListRow; __bg_set : guint);
function selectable(var a : TGtkCListRow) : guint;
procedure set_selectable(var a : TGtkCListRow; __selectable : guint);

function gtk_clist_get_type:TGtkType; cdecl; external gtklib;
{ create a new GtkCList  }
function gtk_clist_new(columns:gint):PGtkWidget; cdecl; external gtklib;
{$IFNDEF KYLIX}
function gtk_clist_new_with_titles(columns:gint; titles:array of Pgchar):PGtkWidget; cdecl; external gtklib;
{$ELSE}
function gtk_clist_new_with_titles(columns:gint):PGtkWidget; varargs; cdecl; external gtklib;
{$ENDIF}
{ set adjustments of clist  }
procedure gtk_clist_set_hadjustment(clist:PGtkCList; adjustment:PGtkAdjustment); cdecl; external gtklib;
procedure gtk_clist_set_vadjustment(clist:PGtkCList; adjustment:PGtkAdjustment); cdecl; external gtklib;
{ get adjustments of clist  }
function gtk_clist_get_hadjustment(clist:PGtkCList):PGtkAdjustment; cdecl; external gtklib;
function gtk_clist_get_vadjustment(clist:PGtkCList):PGtkAdjustment; cdecl; external gtklib;
{ set the border style of the clist  }
procedure gtk_clist_set_shadow_type(clist:PGtkCList; _type:TGtkShadowType); cdecl; external gtklib;
{ set the clist's selection mode  }
procedure gtk_clist_set_selection_mode(clist:PGtkCList; mode:TGtkSelectionMode); cdecl; external gtklib;
{ enable clists reorder ability  }
procedure gtk_clist_set_reorderable(clist:PGtkCList; reorderable:gboolean); cdecl; external gtklib;
procedure gtk_clist_set_use_drag_icons(clist:PGtkCList; use_icons:gboolean); cdecl; external gtklib;
procedure gtk_clist_set_button_actions(clist:PGtkCList; button:guint; button_actions:guint8); cdecl; external gtklib;
{ freeze all visual updates of the list, and then thaw the list after
   you have made a number of changes and the updates wil occure in a
   more efficent mannor than if you made them on a unfrozen list
  }
procedure gtk_clist_freeze(clist:PGtkCList); cdecl; external gtklib;
procedure gtk_clist_thaw(clist:PGtkCList); cdecl; external gtklib;
{ show and hide the column title buttons  }
procedure gtk_clist_column_titles_show(clist:PGtkCList); cdecl; external gtklib;
procedure gtk_clist_column_titles_hide(clist:PGtkCList); cdecl; external gtklib;
{ set the column title to be a active title (responds to button presses,
   prelights, and grabs keyboard focus), or passive where it acts as just
   a title
  }
procedure gtk_clist_column_title_active(clist:PGtkCList; column:gint); cdecl; external gtklib;
procedure gtk_clist_column_title_passive(clist:PGtkCList; column:gint); cdecl; external gtklib;
procedure gtk_clist_column_titles_active(clist:PGtkCList); cdecl; external gtklib;
procedure gtk_clist_column_titles_passive(clist:PGtkCList); cdecl; external gtklib;
{ set the title in the column title button  }
procedure gtk_clist_set_column_title(clist:PGtkCList; column:gint; title:Pgchar); cdecl; external gtklib;
{ returns the title of column. Returns NULL if title is not set  }
function gtk_clist_get_column_title(clist:PGtkCList; column:gint):Pgchar; cdecl; external gtklib;
{ set a widget instead of a title for the column title button  }
procedure gtk_clist_set_column_widget(clist:PGtkCList; column:gint; widget:PGtkWidget); cdecl; external gtklib;
{ returns the column widget  }
function gtk_clist_get_column_widget(clist:PGtkCList; column:gint):PGtkWidget; cdecl; external gtklib;
{ set the justification on a column  }
procedure gtk_clist_set_column_justification(clist:PGtkCList; column:gint; justification:TGtkJustification); cdecl; external gtklib;
{ set visibility of a column  }
procedure gtk_clist_set_column_visibility(clist:PGtkCList; column:gint; visible:gboolean); cdecl; external gtklib;
{ enable/disable column resize operations by mouse  }
procedure gtk_clist_set_column_resizeable(clist:PGtkCList; column:gint; resizeable:gboolean); cdecl; external gtklib;
{ resize column automatically to its optimal width  }
procedure gtk_clist_set_column_auto_resize(clist:PGtkCList; column:gint; auto_resize:gboolean); cdecl; external gtklib;
function gtk_clist_columns_autosize(clist:PGtkCList):gint; cdecl; external gtklib;
{ return the optimal column width, i.e. maximum of all cell widths  }
function gtk_clist_optimal_column_width(clist:PGtkCList; column:gint):gint; cdecl; external gtklib;
{ set the pixel width of a column; this is a necessary step in
   creating a CList because otherwise the column width is chozen from
   the width of the column title, which will never be right
  }
procedure gtk_clist_set_column_width(clist:PGtkCList; column:gint; width:gint); cdecl; external gtklib;
{ set column minimum/maximum width. min/max_width < 0 => no restriction  }
procedure gtk_clist_set_column_min_width(clist:PGtkCList; column:gint; min_width:gint); cdecl; external gtklib;
procedure gtk_clist_set_column_max_width(clist:PGtkCList; column:gint; max_width:gint); cdecl; external gtklib;
{ change the height of the rows, the default (height=0) is
   the hight of the current font.
  }
procedure gtk_clist_set_row_height(clist:PGtkCList; height:guint); cdecl; external gtklib;
{ scroll the viewing area of the list to the given column and row;
   row_align and col_align are between 0-1 representing the location the
   row should appear on the screnn, 0.0 being top or left, 1.0 being
   bottom or right; if row or column is -1 then then there is no change
  }
procedure gtk_clist_moveto(clist:PGtkCList; row:gint; column:gint; row_align:gfloat; col_align:gfloat); cdecl; external gtklib;
{ returns whether the row is visible  }
function gtk_clist_row_is_visible(clist:PGtkCList; row:gint):TGtkVisibility; cdecl; external gtklib;
{ returns the cell type  }
function gtk_clist_get_cell_type(clist:PGtkCList; row:gint; column:gint):TGtkCellType; cdecl; external gtklib;
{ sets a given cell's text, replacing its current contents  }

procedure gtk_clist_set_text(clist:PGtkCList; row:gint; column:gint; text:Pgchar); cdecl; external gtklib;
{ for the "get" functions, any of the return pointer can be
   NULL if you are not interested
  }
function gtk_clist_get_text(clist:PGtkCList; row:gint; column:gint; text:PPgchar):gint; cdecl; external gtklib;
{ sets a given cell's pixmap, replacing its current contents  }
procedure gtk_clist_set_pixmap(clist:PGtkCList; row:gint; column:gint; pixmap:PGdkPixmap; mask:PGdkBitmap); cdecl; external gtklib;
function gtk_clist_get_pixmap(clist:PGtkCList; row:gint; column:gint; var pixmap:PGdkPixmap; var mask:PGdkBitmap):gint; cdecl; external gtklib;
{ sets a given cell's pixmap and text, replacing its current contents  }

procedure gtk_clist_set_pixtext(clist:PGtkCList; row:gint; column:gint; text:Pgchar; spacing:guint8;
            pixmap:PGdkPixmap; mask:PGdkBitmap); cdecl; external gtklib;
function gtk_clist_get_pixtext(clist:PGtkCList; row:gint; column:gint; text:PPgchar; spacing:Pguint8;
           var pixmap:PGdkPixmap; var mask:PGdkBitmap):gint; cdecl; external gtklib;
{ sets the foreground color of a row, the color must already
   be allocated
  }
procedure gtk_clist_set_foreground(clist:PGtkCList; row:gint; color:PGdkColor); cdecl; external gtklib;
{ sets the background color of a row, the color must already
   be allocated
  }
procedure gtk_clist_set_background(clist:PGtkCList; row:gint; color:PGdkColor); cdecl; external gtklib;
{ set / get cell styles  }
procedure gtk_clist_set_cell_style(clist:PGtkCList; row:gint; column:gint; style:PGtkStyle); cdecl; external gtklib;
function gtk_clist_get_cell_style(clist:PGtkCList; row:gint; column:gint):PGtkStyle; cdecl; external gtklib;
procedure gtk_clist_set_row_style(clist:PGtkCList; row:gint; style:PGtkStyle); cdecl; external gtklib;
function gtk_clist_get_row_style(clist:PGtkCList; row:gint):PGtkStyle; cdecl; external gtklib;
{ this sets a horizontal and vertical shift for drawing
   the contents of a cell; it can be positive or negitive;
   this is particulary useful for indenting items in a column
  }
procedure gtk_clist_set_shift(clist:PGtkCList; row:gint; column:gint; vertical:gint; horizontal:gint); cdecl; external gtklib;
{ set/get selectable flag of a single row  }
procedure gtk_clist_set_selectable(clist:PGtkCList; row:gint; selectable:gboolean); cdecl; external gtklib;
function gtk_clist_get_selectable(clist:PGtkCList; row:gint):gboolean; cdecl; external gtklib;
{ prepend/append returns the index of the row you just added,
   making it easier to append and modify a row
  }
{$IFNDEF KYLIX}
function gtk_clist_prepend(clist:PGtkCList; text:PPgchar):gint; cdecl; external gtklib;
function gtk_clist_append(clist:PGtkCList; text:PPgchar):gint; cdecl; external gtklib;
{$ELSE}
function gtk_clist_prepend(clist:PGtkCList):gint; varargs; cdecl; external gtklib;
function gtk_clist_append(clist:PGtkCList):gint; varargs; cdecl; external gtklib;
{$ENDIF}
{ inserts a row at index row and returns the row where it was
   actually inserted (may be different from "row" in auto_sort mode)
  }
{$IFNDEF KYLIX}
function gtk_clist_insert(clist:PGtkCList; row:gint; text:PPgchar):gint; cdecl; external gtklib;
{$ELSE}
function gtk_clist_insert(clist:PGtkCList; row:gint):gint; varargs; cdecl; external gtklib;
{$ENDIF}
{ removes row at index row  }
procedure gtk_clist_remove(clist:PGtkCList; row:gint); cdecl; external gtklib;
{ sets a arbitrary data pointer for a given row  }
procedure gtk_clist_set_row_data(clist:PGtkCList; row:gint; data:gpointer); cdecl; external gtklib;
{ sets a data pointer for a given row with destroy notification  }
procedure gtk_clist_set_row_data_full(clist:PGtkCList; row:gint; data:gpointer; destroy:TGtkDestroyNotify); cdecl; external gtklib;
{ returns the data set for a row  }
function gtk_clist_get_row_data(clist:PGtkCList; row:gint):gpointer; cdecl; external gtklib;
{ givin a data pointer, find the first (and hopefully only!)
   row that points to that data, or -1 if none do
  }
function gtk_clist_find_row_from_data(clist:PGtkCList; data:gpointer):gint; cdecl; external gtklib;
{ force selection of a row  }
procedure gtk_clist_select_row(clist:PGtkCList; row:gint; column:gint); cdecl; external gtklib;
{ force unselection of a row  }
procedure gtk_clist_unselect_row(clist:PGtkCList; row:gint; column:gint); cdecl; external gtklib;
{ undo the last select/unselect operation  }
procedure gtk_clist_undo_selection(clist:PGtkCList); cdecl; external gtklib;
{ clear the entire list -- this is much faster than removing
   each item with gtk_clist_remove
  }
procedure gtk_clist_clear(clist:PGtkCList); cdecl; external gtklib;
{ return the row column corresponding to the x and y coordinates,
   the returned values are only valid if the x and y coordinates
   are respectively to a window == clist->clist_window
  }
function gtk_clist_get_selection_info(clist:PGtkCList; x:gint; y:gint; row:Pgint; column:Pgint):gint; cdecl; external gtklib;
{ in multiple or extended mode, select all rows  }
procedure gtk_clist_select_all(clist:PGtkCList); cdecl; external gtklib;
{ in all modes except browse mode, deselect all rows  }
procedure gtk_clist_unselect_all(clist:PGtkCList); cdecl; external gtklib;
{ swap the position of two rows  }
procedure gtk_clist_swap_rows(clist:PGtkCList; row1:gint; row2:gint); cdecl; external gtklib;
{ move row from source_row position to dest_row position  }
procedure gtk_clist_row_move(clist:PGtkCList; source_row:gint; dest_row:gint); cdecl; external gtklib;
{ sets a compare function different to the default  }
procedure gtk_clist_set_compare_func(clist:PGtkCList; cmp_func:TGtkCListCompareFunc); cdecl; external gtklib;
{ the column to sort by  }
procedure gtk_clist_set_sort_column(clist:PGtkCList; column:gint); cdecl; external gtklib;
{ how to sort : ascending or descending  }
procedure gtk_clist_set_sort_type(clist:PGtkCList; sort_type:TGtkSortType); cdecl; external gtklib;
{ sort the list with the current compare function  }
procedure gtk_clist_sort(clist:PGtkCList); cdecl; external gtklib;
{ Automatically sort upon insertion  }
procedure gtk_clist_set_auto_sort(clist:PGtkCList; auto_sort:gboolean); cdecl; external gtklib;
{ Private function for clist, ctree  }
function _gtk_clist_create_cell_layout(clist:PGtkCList; clist_row:PGtkCListRow; column:gint):PPangoLayout; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CLIST : GType;
begin
   GTK_TYPE_CLIST:=gtk_clist_get_type;
end;

function GTK_CLIST(obj: pointer) : PGtkCList;
begin
   GTK_CLIST:=PGtkCList(GTK_CHECK_CAST(obj,GTK_TYPE_CLIST));
end;

function GTK_CLIST_CLASS(klass: pointer) : PGtkCListClass;
begin
   GTK_CLIST_CLASS:=PGtkCListClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_CLIST));
end;

function GTK_IS_CLIST(obj: pointer) : boolean;
begin
   GTK_IS_CLIST:=GTK_CHECK_TYPE(obj,GTK_TYPE_CLIST);
end;

function GTK_IS_CLIST_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_CLIST_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_CLIST);
end;

function GTK_CLIST_GET_CLASS(obj: pointer) : PGtkCListClass;
begin
   GTK_CLIST_GET_CLASS:=PGtkCListClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_CLIST));
end;

function GTK_CLIST_FLAGS(clist : pointer) : guint16;
begin
   GTK_CLIST_FLAGS:=GTK_CLIST(clist)^.flags;
end;

procedure GTK_CLIST_SET_FLAG(clist: PGtkCList; flag: guint16);
begin
  clist^.flags:=GTK_CLIST(clist)^.flags or flag;
end;

procedure GTK_CLIST_UNSET_FLAG(clist: PGtkCList; flag: guint16);
begin
  clist^.flags:=GTK_CLIST(clist)^.flags and not flag;
end;

function GTK_CLIST_IN_DRAG_get(clist : pointer) : boolean;
begin
   GTK_CLIST_IN_DRAG_get:=((GTK_CLIST_FLAGS(clist)) and GTK_CLIST_IN_DRAG)<>0;
end;

function GTK_CLIST_ROW_HEIGHT_SET_get(clist : pointer) : boolean;
begin
   GTK_CLIST_ROW_HEIGHT_SET_get:=((GTK_CLIST_FLAGS(clist)) and GTK_CLIST_ROW_HEIGHT_SET)<>0;
end;

function GTK_CLIST_SHOW_TITLES_get(clist : pointer) : boolean;
begin
   GTK_CLIST_SHOW_TITLES_get:=((GTK_CLIST_FLAGS(clist)) and GTK_CLIST_SHOW_TITLES)<>0;
end;

function GTK_CLIST_ADD_MODE_get(clist : pointer) : boolean;
begin
   GTK_CLIST_ADD_MODE_get:=((GTK_CLIST_FLAGS(clist)) and GTK_CLIST_ADD_MODE)<>0;
end;

function GTK_CLIST_AUTO_SORT_get(clist : pointer) : boolean;
begin
   GTK_CLIST_AUTO_SORT_get:=((GTK_CLIST_FLAGS(clist)) and GTK_CLIST_AUTO_SORT)<>0;
end;

function GTK_CLIST_AUTO_RESIZE_BLOCKED_get(clist : pointer) : boolean;
begin
   GTK_CLIST_AUTO_RESIZE_BLOCKED_get:=((GTK_CLIST_FLAGS(clist)) and GTK_CLIST_AUTO_RESIZE_BLOCKED)<>0;
end;

function GTK_CLIST_REORDERABLE_get(clist : pointer) : boolean;
begin
   GTK_CLIST_REORDERABLE_get:=((GTK_CLIST_FLAGS(clist)) and GTK_CLIST_REORDERABLE)<>0;
end;

function GTK_CLIST_USE_DRAG_ICONS_get(clist : pointer) : boolean;
begin
   GTK_CLIST_USE_DRAG_ICONS_get:=((GTK_CLIST_FLAGS(clist)) and GTK_CLIST_USE_DRAG_ICONS)<>0;
end;

function GTK_CLIST_DRAW_DRAG_LINE_get(clist : pointer) : boolean;
begin
   GTK_CLIST_DRAW_DRAG_LINE_get:=((GTK_CLIST_FLAGS(clist)) and GTK_CLIST_DRAW_DRAG_LINE)<>0;
end;

function GTK_CLIST_DRAW_DRAG_RECT_get(clist : pointer) : boolean;
begin
   GTK_CLIST_DRAW_DRAG_RECT_get:=((GTK_CLIST_FLAGS(clist)) and GTK_CLIST_DRAW_DRAG_RECT)<>0;
end;

function GTK_CLIST_ROW_get(_glist_ : PGList) : PGtkCListRow;
begin
   GTK_CLIST_ROW_get:=PGtkCListRow(_glist_^.data);
end;

function GTK_CELL_TEXT_get(cell : pointer) : PGtkCellText;
begin
   GTK_CELL_TEXT_get:=PGtkCellText(@(cell));
end;

function GTK_CELL_PIXMAP_get(cell : pointer) : PGtkCellPixmap;
begin
   GTK_CELL_PIXMAP_get:=PGtkCellPixmap(@(cell));
end;

function GTK_CELL_PIXTEXT_get(cell : pointer) : PGtkCellPixText;
begin
   GTK_CELL_PIXTEXT_get:=PGtkCellPixText(@(cell));
end;

function GTK_CELL_WIDGET_get(cell : pointer) : PGtkCellWidget;
begin
   GTK_CELL_WIDGET_get:=PGtkCellWidget(@(cell));
end;

function visible(var a : TGtkCListColumn) : guint;
begin
   visible:=(a.flag0 and bm_TGtkCListColumn_visible) shr bp_TGtkCListColumn_visible;
end;

procedure set_visible(var a : TGtkCListColumn; __visible : guint);
begin
   a.flag0:=a.flag0 or ((__visible shl bp_TGtkCListColumn_visible) and bm_TGtkCListColumn_visible);
end;

function width_set(var a : TGtkCListColumn) : guint;
begin
   width_set:=(a.flag0 and bm_TGtkCListColumn_width_set) shr bp_TGtkCListColumn_width_set;
end;

procedure set_width_set(var a : TGtkCListColumn; __width_set : guint);
begin
   a.flag0:=a.flag0 or ((__width_set shl bp_TGtkCListColumn_width_set) and bm_TGtkCListColumn_width_set);
end;

function resizeable(var a : TGtkCListColumn) : guint;
begin
   resizeable:=(a.flag0 and bm_TGtkCListColumn_resizeable) shr bp_TGtkCListColumn_resizeable;
end;

procedure set_resizeable(var a : TGtkCListColumn; __resizeable : guint);
begin
   a.flag0:=a.flag0 or ((__resizeable shl bp_TGtkCListColumn_resizeable) and bm_TGtkCListColumn_resizeable);
end;

function auto_resize(var a : TGtkCListColumn) : guint;
begin
   auto_resize:=(a.flag0 and bm_TGtkCListColumn_auto_resize) shr bp_TGtkCListColumn_auto_resize;
end;

procedure set_auto_resize(var a : TGtkCListColumn; __auto_resize : guint);
begin
   a.flag0:=a.flag0 or ((__auto_resize shl bp_TGtkCListColumn_auto_resize) and bm_TGtkCListColumn_auto_resize);
end;

function button_passive(var a : TGtkCListColumn) : guint;
begin
   button_passive:=(a.flag0 and bm_TGtkCListColumn_button_passive) shr bp_TGtkCListColumn_button_passive;
end;

procedure set_button_passive(var a : TGtkCListColumn; __button_passive : guint);
begin
   a.flag0:=a.flag0 or ((__button_passive shl bp_TGtkCListColumn_button_passive) and bm_TGtkCListColumn_button_passive);
end;

function fg_set(var a : TGtkCListRow) : guint;
begin
   fg_set:=(a.flag0 and bm_TGtkCListRow_fg_set) shr bp_TGtkCListRow_fg_set;
end;

procedure set_fg_set(var a : TGtkCListRow; __fg_set : guint);
begin
   a.flag0:=a.flag0 or ((__fg_set shl bp_TGtkCListRow_fg_set) and bm_TGtkCListRow_fg_set);
end;

function bg_set(var a : TGtkCListRow) : guint;
begin
   bg_set:=(a.flag0 and bm_TGtkCListRow_bg_set) shr bp_TGtkCListRow_bg_set;
end;

procedure set_bg_set(var a : TGtkCListRow; __bg_set : guint);
begin
   a.flag0:=a.flag0 or ((__bg_set shl bp_TGtkCListRow_bg_set) and bm_TGtkCListRow_bg_set);
end;

function selectable(var a : TGtkCListRow) : guint;
begin
   selectable:=(a.flag0 and bm_TGtkCListRow_selectable) shr bp_TGtkCListRow_selectable;
end;

procedure set_selectable(var a : TGtkCListRow; __selectable : guint);
begin
   a.flag0:=a.flag0 or ((__selectable shl bp_TGtkCListRow_selectable) and bm_TGtkCListRow_selectable);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

