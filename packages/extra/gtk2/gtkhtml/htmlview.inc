// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PHtmlView = ^THtmlView;
  THtmlView = record
    parent : TGtkLayout;
    document : PHtmlDocument;
    //root : PHtmlBox;
    node_table : PGHashTable;
    //painter : PHtmlPainter;
    relayout_idle_id : guint;
    relayout_timeout_id : guint;
    mouse_down_x : gint;
    mouse_down_y : gint;
    mouse_detail : gint;
    //sel_start : PHtmlBox;
    sel_start_ypos : gint;
    sel_start_index : gint;
    //sel_end : PHtmlBox;
    sel_end_ypos : gint;
    sel_end_index : gint;
    sel_flag : gboolean;
    sel_backwards : gboolean;
    sel_start_found : gboolean;
    sel_list : PGSList;
    jump_to_anchor : pgchar;
    magnification : gdouble;
    magnification_modified : gboolean;
    on_url : gboolean;
  end;//THtmlView

  PHtmlViewClass = ^THtmlViewClass;
  THtmlViewClass = record
    parent : TGtkLayoutClass;
    move_cursor : procedure (html_view:PHtmlView; step:TGtkMovementStep; count:gint; extend_selection:gboolean);cdecl;
    //request_object : function (html_view:PHtmlView; widget:PHtmlEmbedded):gboolean;
    on_url : procedure (html_view:PHtmlView; url:Pgchar);
    activate : procedure (html_view:PHtmlView);
    move_focus_out : procedure (html_view:PHtmlView; direction:TGtkDirectionType);
  end;//THtmlViewClass


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  function HTML_TYPE_VIEW : GType;
  function HTML_VIEW(obj : pointer) : PHtmlView;
  function HTML_VIEW_CLASS(klass : pointer) : PHtmlViewClass;
  function HTML_IS_VIEW(obj : pointer) : boolean;

  function html_view_get_type:GType;cdecl; external gtkhtmllib;
  function html_view_new:PGtkWidget;cdecl; external gtkhtmllib;
  procedure html_view_set_document(view:PHtmlView; document:PHtmlDocument);cdecl; external gtkhtmllib;
  procedure html_view_jump_to_anchor(view:PHtmlView; anchor:Pgchar);cdecl; external gtkhtmllib;
  function html_view_get_magnification(view:PHtmlView):gdouble;cdecl; external gtkhtmllib;
  procedure html_view_set_magnification(view:PHtmlView; magnification:gdouble);cdecl; external gtkhtmllib;
  procedure html_view_zoom_in(view:PHtmlView);cdecl; external gtkhtmllib;
  procedure html_view_zoom_out(view:PHtmlView);cdecl; external gtkhtmllib;
  procedure html_view_zoom_reset(view:PHtmlView);cdecl; external gtkhtmllib;
  //function html_view_find_layout_box(view:PHtmlView; node:PDomNode; find_parent:gboolean):PHtmlBox;cdecl; external gtkhtmllib;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

function HTML_TYPE_VIEW : GType;
begin
  HTML_TYPE_VIEW:=html_view_get_type;
end;//HTML_TYPE_VIEW

function HTML_VIEW(obj : pointer) : PHtmlView;
begin
  HTML_VIEW:=PHtmlView(GTK_CHECK_CAST(obj,HTML_TYPE_VIEW));
end;//HTML_VIEW

function HTML_VIEW_CLASS(klass : pointer) : PHtmlViewClass;
begin
  HTML_VIEW_CLASS:=PHtmlViewClass(GTK_CHECK_CLASS_CAST(klass,HTML_TYPE_VIEW));
end;//HTML_VIEW_CLASS

function HTML_IS_VIEW(obj : pointer) : boolean;
begin
  HTML_IS_VIEW:=GTK_CHECK_TYPE(obj,HTML_TYPE_VIEW);
end;//HTML_IS_VIEW


{$ENDIF read_implementation}
// included by gtkhtml.pas
