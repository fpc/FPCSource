// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

  THtmlParserType = (HTML_PARSER_TYPE_HTML,HTML_PARSER_TYPE_XML);

  PHtmlParser = ^THtmlParser;
  THtmlParser = record
    parent : TGObject;
    parser_type : THtmlParserType;
    document : PHtmlDocument;
    stream : PHtmlStream;
    xmlctxt : xmlParserCtxtPtr;
    res : longint;
    chars : array[0..9] of char;
    blocking : gboolean;
    blocking_node : PDomNode;
  end;//THtmlParser

  PHtmlParserClass = ^THtmlParserClass;
  THtmlParserClass = record
    parent : TGtkObjectClass;
    done_parsing : procedure (parser:PHtmlParser);cdecl;
    new_node : procedure (parser:PHtmlParser; node:PDomNode);
    parsed_document_node : procedure (parser:PHtmlParser; document:PDomDocument);
  end;//THtmlParserClass

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  function HTML_PARSER_TYPE : GType;
  function HTML_PARSER(obj : pointer) : PHtmlParser;
  function HTML_PARSER_CLASS(klass : pointer) : PHtmlParserClass;
  function HTML_IS_PARSER(obj : pointer) : boolean;

  function html_parser_get_type:GType;
  function html_parser_new(document:PHtmlDocument; parser_type:THtmlParserType):PHtmlParser;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

  function HTML_PARSER_TYPE : GType;
  begin
    HTML_PARSER_TYPE:=html_parser_get_type;
  end;//HTML_PARSER_TYPE

  function HTML_PARSER(obj : pointer) : PHtmlParser;
  begin
    HTML_PARSER:=GTK_CHECK_CAST(obj,HTML_PARSER_TYPE,THtmlParser);
  end;//HTML_PARSER

  function HTML_PARSER_CLASS(klass : pointer) : PHtmlParserClass;
  begin
    HTML_PARSER_CLASS:=GTK_CHECK_CLASS_CAST(klass,HTML_PARSER_TYPE,THtmlParserClass);
  end;//HTML_PARSER_CLASS

  function HTML_IS_PARSER(obj : pointer) : boolean;
  begin
    HTML_IS_PARSER:=GTK_CHECK_TYPE(obj,HTML_PARSER_TYPE);
  end;//HTML_IS_PARSER

{$ENDIF read_implementation}
// included by gtkhtml.pas
