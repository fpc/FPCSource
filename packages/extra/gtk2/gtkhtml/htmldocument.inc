// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

  THtmlDocumentState = (HTML_DOCUMENT_STATE_DONE,HTML_DOCUMENT_STATE_PARSING);

  PHtmlDocument = ^THtmlDocument;
  THtmlDocument = record
    parent_instance : TGObject;
    //dom_document : PDomDocument;
    stylesheets : PGSList;
    //parser : PHtmlParser;
    current_stream : PHtmlStream;
    //image_factory : PHtmlImageFactory;
    state : THtmlDocumentState;
    //hover_node : PDomNode;
    //active_node : PDomNode;
    //focus_element : PDomElement;
  end;//THtmlDocument

  PHtmlDocumentClass = ^THtmlDocumentClass;
  THtmlDocumentClass = record
    parent_class : TGObjectClass;
    request_url : procedure (document:PHtmlDocument; url:Pgchar; stream:PHtmlStream);cdecl;
    link_clicked : procedure (document:PHtmlDocument; url:Pgchar);cdecl;
    set_base : procedure (document:PHtmlDocument; url:Pgchar);cdecl;
    title_changed : procedure (document:PHtmlDocument; new_title:Pgchar);cdecl;
    submit : procedure (document:PHtmlDocument; method:Pgchar; url:Pgchar; encoding:Pgchar);cdecl;
    //node_inserted : procedure (document:PHtmlDocument; node:PDomNode);cdecl;
    //node_removed : procedure (document:PHtmlDocument; node:PDomNode);cdecl;
    //text_updated : procedure (document:PHtmlDocument; node:PDomNode);cdecl;
    //style_updated : procedure (document:PHtmlDocument; node:PDomNode; style_change:THtmlStyleChange);cdecl;
    //relayout_node : procedure (document:PHtmlDocument; node:PDomNode);cdecl;
    //repaint_node : procedure (document:PHtmlDocument; node:PDomNode);cdecl;
    //dom_mouse_down : function (document:PHtmlDocument; event:PDomEvent):gboolean;cdecl;
    //dom_mouse_up : function (document:PHtmlDocument; event:PDomEvent):gboolean;cdecl;
    //dom_mouse_click : function (document:PHtmlDocument; event:PDomEvent):gboolean;cdecl;
    //dom_mouse_over : function (document:PHtmlDocument; event:PDomEvent):gboolean;cdecl;
    //dom_mouse_out : function (document:PHtmlDocument; event:PDomEvent):gboolean;cdecl;
  end;//THtmlDocumentClass

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  function HTML_TYPE_DOCUMENT : GType;
  function HTML_DOCUMENT(obj : pointer) : PHtmlDocument;
  function HTML_DOCUMENT_CLASS(klass : pointer) : PHtmlDocumentClass;
  function HTML_IS_DOCUMENT(obj : pointer) : boolean;

  function html_document_get_type:GType;cdecl; external gtkhtmllib;
  function html_document_new:PHtmlDocument;cdecl; external gtkhtmllib;
  function html_document_open_stream(document:PHtmlDocument; mime_type:Pgchar):gboolean;cdecl; external gtkhtmllib;
  procedure html_document_write_stream(document:PHtmlDocument; buffer:Pgchar; len:gint);cdecl; external gtkhtmllib;
  procedure html_document_close_stream(document:PHtmlDocument);cdecl; external gtkhtmllib;
  procedure html_document_clear(document:PHtmlDocument);cdecl; external gtkhtmllib;
  //procedure html_document_update_hover_node(document:PHtmlDocument; node:PDomNode);cdecl; external gtkhtmllib;
  //procedure html_document_update_active_node(document:PHtmlDocument; node:PDomNode);cdecl; external gtkhtmllib;
  //procedure html_document_update_focus_element(document:PHtmlDocument; element:PDomElement);cdecl; external gtkhtmllib;
  //function html_document_find_anchor(doc:PHtmlDocument; anchor:Pgchar):PDomNode;cdecl; external gtkhtmllib;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

  function HTML_TYPE_DOCUMENT : GType;
  begin
    HTML_TYPE_DOCUMENT:=html_document_get_type;
  end;//HTML_TYPE_DOCUMENT

  function HTML_DOCUMENT(obj : pointer) : PHtmlDocument;
  begin
    HTML_DOCUMENT:=PHtmlDocument(GTK_CHECK_CAST(obj,HTML_TYPE_DOCUMENT));
  end;//HTML_DOCUMENT

  function HTML_DOCUMENT_CLASS(klass : pointer) : PHtmlDocumentClass;
  begin
    HTML_DOCUMENT_CLASS:=GTK_CHECK_CLASS_CAST(klass,HTML_TYPE_DOCUMENT);
  end;//HTML_DOCUMENT_CLASS

  function HTML_IS_DOCUMENT(obj : pointer) : boolean;
  begin
    HTML_IS_DOCUMENT:=GTK_CHECK_TYPE(obj,HTML_TYPE_DOCUMENT);
  end;//HTML_IS_DOCUMENT

{$ENDIF read_implementation}
// included by gtkhtml.pas
