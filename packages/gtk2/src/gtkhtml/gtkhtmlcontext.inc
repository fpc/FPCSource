// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

  PGtkHtmlContext = ^TGtkHtmlContext;
  TGtkHtmlContext = record
    parent : TGObject;
    documents : PGSList;
    standard_font : PHtmlFontSpecification;
    fixed_font : PHtmlFontSpecification;
    debug_painting : gboolean;
  end;//TGtkHtmlContext

  PGtkHtmlContextClass = ^TGtkHtmlContextClass;
  TGtkHtmlContextClass = record
    parent : TGObjectClass;
  end;//TGtkHtmlContextClass

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  function GTK_HTML_CONTEXT_TYPE : GType;
  function GTK_HTML_CONTEXT(obj : pointer) : PGtkHtmlContext;
  function GTK_HTML_CONTEXT_CLASS(klass : pointer) : PGtkHtmlContextClass;
  function GTK_HTML_IS_CONTEXT(obj : pointer) : boolean;
  function GTK_HTML_IS_CONTEXT_CLASS(klass : pointer) : boolean;

  function gtk_html_context_get_type:GType;
  function gtk_html_context_get:PGtkHtmlContext;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

  function GTK_HTML_CONTEXT_TYPE : GType;
  begin
    GTK_HTML_CONTEXT_TYPE:=gtk_html_context_get_type;
  end;//GTK_HTML_CONTEXT_TYPE

  function GTK_HTML_CONTEXT(obj : pointer) : PGtkHtmlContext;
  begin
    GTK_HTML_CONTEXT:=GTK_CHECK_CAST(obj,GTK_HTML_CONTEXT_TYPE,TGtkHtmlContext);
  end;//GTK_HTML_CONTEXT

  function GTK_HTML_CONTEXT_CLASS(klass : pointer) : PGtkHtmlContextClass;
  begin
    GTK_HTML_CONTEXT_CLASS:=GTK_CHECK_CLASS_CAST(klass,GTK_HTML_CONTEXT_TYPE,TGtkHtmlContextClass);
  end;//GTK_HTML_CONTEXT_CLASS

  function GTK_HTML_IS_CONTEXT(obj : pointer) : boolean;
  begin
    GTK_HTML_IS_CONTEXT:=GTK_CHECK_TYPE(obj,GTK_HTML_CONTEXT_TYPE);
  end;//GTK_HTML_IS_CONTEXT

  function GTK_HTML_IS_CONTEXT_CLASS(klass : pointer) : boolean;
  begin
    GTK_HTML_IS_CONTEXT_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_HTML_CONTEXT_TYPE);
  end;//GTK_HTML_IS_CONTEXT_CLASS

{$ENDIF read_implementation}
// included by gtkhtml.pas
