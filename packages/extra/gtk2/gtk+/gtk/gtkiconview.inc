// included by gtk2.pas

{$IFDEF read_forward_definitions}



{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkIconViewPrivate = gpointer;
   PGtkIconView = ^TGtkIconView;
   TGtkIconView = record
     parent : TGtkContainer;
     priv : PGtkIconViewPrivate;
   end;

{ Key Binding signals  }
{ Padding for future expansion  }
   PGtkIconViewClass = ^TGtkIconViewClass;
       TGtkIconViewClass = record
            parent_class : TGtkContainerClass;
            set_scroll_adjustments : procedure (icon_view:PGtkIconView; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment);cdecl;
            item_activated : procedure (icon_view:PGtkIconView; path:PGtkTreePath);cdecl;
            selection_changed : procedure (icon_view:PGtkIconView);cdecl;
            select_all : procedure (icon_view:PGtkIconView);cdecl;
            unselect_all : procedure (icon_view:PGtkIconView);cdecl;
            select_cursor_item : procedure (icon_view:PGtkIconView);cdecl;
            toggle_cursor_item : procedure (icon_view:PGtkIconView);cdecl;
            move_cursor : function (icon_view:PGtkIconView; step:TGtkMovementStep; count:gint):gboolean;cdecl;
            activate_cursor_item : function (icon_view:PGtkIconView):gboolean;cdecl;
         end;

         TGtkIconViewForeachFunc = procedure (icon_view:PGtkIconView; path:PGtkTreePath; data:gpointer);cdecl;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_ICON_VIEW : GType;
function GTK_ICON_VIEW(obj: pointer) : PGtkIconView;
function GTK_ICON_VIEW_CLASS(klass: pointer) : PGtkIconViewClass;
function GTK_IS_ICON_VIEW(obj: pointer) : boolean;
function GTK_IS_ICON_VIEW_CLASS(klass: pointer) : boolean;
function GTK_ICON_VIEW_GET_CLASS(obj: pointer) : PGtkIconViewClass;

{ Creators  }

function gtk_icon_view_get_type:TGtkType; cdecl; external gtklib;
function gtk_icon_view_new:PGtkWidget;cdecl;external gtklib;
function gtk_icon_view_new_with_model(model:PGtkTreeModel):PGtkWidget;cdecl;external gtklib;

procedure gtk_icon_view_set_model(icon_view:PGtkIconView; model:PGtkTreeModel);cdecl;external gtklib;
function gtk_icon_view_get_model(icon_view:PGtkIconView):PGtkTreeModel;cdecl;external gtklib;
procedure gtk_icon_view_set_text_column(icon_view:PGtkIconView; column:gint);cdecl;external gtklib;
function gtk_icon_view_get_text_column(icon_view:PGtkIconView):gint;cdecl;external gtklib;
procedure gtk_icon_view_set_markup_column(icon_view:PGtkIconView; column:gint);cdecl;external gtklib;
function gtk_icon_view_get_markup_column(icon_view:PGtkIconView):gint;cdecl;external gtklib;
procedure gtk_icon_view_set_pixbuf_column(icon_view:PGtkIconView; column:gint);cdecl;external gtklib;
function gtk_icon_view_get_pixbuf_column(icon_view:PGtkIconView):gint;cdecl;external gtklib;
procedure gtk_icon_view_set_orientation(icon_view:PGtkIconView; orientation:TGtkOrientation);cdecl;external gtklib;
function gtk_icon_view_get_orientation(icon_view:PGtkIconView):TGtkOrientation;cdecl;external gtklib;
procedure gtk_icon_view_set_columns(icon_view:PGtkIconView; columns:gint);cdecl;external gtklib;
function gtk_icon_view_get_columns(icon_view:PGtkIconView):gint;cdecl;external gtklib;
procedure gtk_icon_view_set_item_width(icon_view:PGtkIconView; item_width:gint);cdecl;external gtklib;
function gtk_icon_view_get_item_width(icon_view:PGtkIconView):gint;cdecl;external gtklib;
procedure gtk_icon_view_set_spacing(icon_view:PGtkIconView; spacing:gint);cdecl;external gtklib;
function gtk_icon_view_get_spacing(icon_view:PGtkIconView):gint;cdecl;external gtklib;
procedure gtk_icon_view_set_row_spacing(icon_view:PGtkIconView; row_spacing:gint);cdecl;external gtklib;
function gtk_icon_view_get_row_spacing(icon_view:PGtkIconView):gint;cdecl;external gtklib;
procedure gtk_icon_view_set_column_spacing(icon_view:PGtkIconView; column_spacing:gint);cdecl;external gtklib;
function gtk_icon_view_get_column_spacing(icon_view:PGtkIconView):gint;cdecl;external gtklib;
procedure gtk_icon_view_set_margin(icon_view:PGtkIconView; margin:gint);cdecl;external gtklib;
function gtk_icon_view_get_margin(icon_view:PGtkIconView):gint;cdecl;external gtklib;
function gtk_icon_view_get_path_at_pos(icon_view:PGtkIconView; x:gint; y:gint):PGtkTreePath;cdecl;external gtklib;
procedure gtk_icon_view_selected_foreach(icon_view:PGtkIconView; func:TGtkIconViewForeachFunc; data:gpointer);cdecl;external gtklib;
procedure gtk_icon_view_set_selection_mode(icon_view:PGtkIconView; mode:TGtkSelectionMode);cdecl;external gtklib;
function gtk_icon_view_get_selection_mode(icon_view:PGtkIconView):TGtkSelectionMode;cdecl;external gtklib;
procedure gtk_icon_view_select_path(icon_view:PGtkIconView; path:PGtkTreePath);cdecl;external gtklib;
procedure gtk_icon_view_unselect_path(icon_view:PGtkIconView; path:PGtkTreePath);cdecl;external gtklib;
function gtk_icon_view_path_is_selected(icon_view:PGtkIconView; path:PGtkTreePath):gboolean;cdecl;external gtklib;
function gtk_icon_view_get_selected_items(icon_view:PGtkIconView):PGList;cdecl;external gtklib;
procedure gtk_icon_view_select_all(icon_view:PGtkIconView);cdecl;external gtklib;
procedure gtk_icon_view_unselect_all(icon_view:PGtkIconView);cdecl;external gtklib;
procedure gtk_icon_view_item_activated(icon_view:PGtkIconView; path:PGtkTreePath);cdecl;external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ICON_VIEW : GType;
begin
   GTK_TYPE_ICON_VIEW:=gtk_icon_view_get_type;
end;

function GTK_ICON_VIEW(obj: pointer) : PGtkIconView;
begin
   GTK_ICON_VIEW:=PGtkIconView(GTK_CHECK_CAST(obj,GTK_TYPE_ICON_VIEW));
end;

function GTK_ICON_VIEW_CLASS(klass: pointer) : PGtkIconViewClass;
begin
   GTK_ICON_VIEW_CLASS:=PGtkIconViewClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_ICON_VIEW));
end;

function GTK_IS_ICON_VIEW(obj: pointer) : boolean;
begin
   GTK_IS_ICON_VIEW:=GTK_CHECK_TYPE(obj,GTK_TYPE_ICON_VIEW);
end;

function GTK_IS_ICON_VIEW_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ICON_VIEW_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_ICON_VIEW);
end;

function GTK_ICON_VIEW_GET_CLASS(obj: pointer) : PGtkIconViewClass;
begin
   GTK_ICON_VIEW_GET_CLASS:=PGtkIconViewClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_ICON_VIEW));
end;

{$ENDIF read_implementation}
// included by gtk2.pas
