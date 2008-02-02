// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

  PDomNode = ^TDomNode;
  TDomNode = record
    parent : TGObject;
    xmlnode : PxmlNode;
    style : PHtmlStyle;
  end;//TDomNode

  PDomNodeClass =^TDomNodeClass;
  TDomNodeClass = record
    parent_class : TGObjectClass;
    _get_nodeName : function (node:PDomNode):PDomString;cdecl;
    _get_nodeValue : function (node:PDomNode; exc:PDomException):PDomString;cdecl;
    _set_nodeValue : function (node:PDomNode; value:PDomString; exc:PDomException):PDomString;cdecl;
  end;//TDomNodeClass

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  function DOM_TYPE_NODE : GType;
  function DOM_NODE(object : pointer) : PDomNode;
  function DOM_NODE_CLASS(klass : pointer) : PDomNodeClass;
  function DOM_IS_NODE(object : pointer) : boolean;
  function DOM_IS_NODE_CLASS(klass : pointer) : boolean;
  function DOM_NODE_GET_CLASS(obj : pointer) : longint;

  function dom_node_get_type:GType;cdecl; external gtkhtmllib;
  function dom_Node_mkref(node:PxmlNode):PDomNode;cdecl; external gtkhtmllib;
  function dom_Node__get_childNodes(node:PDomNode):PDomNodeList;cdecl; external gtkhtmllib;
  function dom_Node_removeChild(node:PDomNode; oldChild:PDomNode; exc:PDomException):PDomNode;cdecl; external gtkhtmllib;
  function dom_Node__get_nodeValue(node:PDomNode; exc:PDomException):PDomString;cdecl; external gtkhtmllib;
  function dom_Node__get_firstChild(node:PDomNode):PDomNode;cdecl; external gtkhtmllib;
  function dom_Node__get_nodeName(node:PDomNode):PDomString;cdecl; external gtkhtmllib;
  function dom_Node__get_attributes(node:PDomNode):PDomNamedNodeMap;cdecl; external gtkhtmllib;
  function dom_Document__get_doctype(doc:PDomDocument):PDomDocumentType;cdecl; external gtkhtmllib;
  function dom_Node_hasChildNodes(node:PDomNode):DomBoolean;cdecl; external gtkhtmllib;
  function dom_Node__get_parentNode(node:PDomNode):PDomNode;cdecl; external gtkhtmllib;
  function dom_Node__get_nextSibling(node:PDomNode):PDomNode;cdecl; external gtkhtmllib;
  function dom_Node__get_nodeType(node:PDomNode):gushort;cdecl; external gtkhtmllib;
  function dom_Node_hasAttributes(node:PDomNode):DomBoolean;cdecl; external gtkhtmllib;
  function dom_Node_cloneNode(node:PDomNode; deep:DomBoolean):PDomNode;cdecl; external gtkhtmllib;
  function dom_Node_appendChild(node:PDomNode; newChild:PDomNode; exc:PDomException):PDomNode;cdecl; external gtkhtmllib;
  function dom_Node__get_localName(node:PDomNode):PDomString;cdecl; external gtkhtmllib;
  function dom_Node__get_namespaceURI(node:PDomNode):PDomString;cdecl; external gtkhtmllib;
  function dom_Node__get_previousSibling(node:PDomNode):PDomNode;cdecl; external gtkhtmllib;
  function dom_Node__get_lastChild(node:PDomNode):PDomNode;cdecl; external gtkhtmllib;
  procedure dom_Node__set_nodeValue(node:PDomNode; value:PDomString; exc:PDomException);cdecl; external gtkhtmllib;
  function dom_Node__get_ownerDocument(node:PDomNode):PDomDocument;cdecl; external gtkhtmllib;
  function dom_Node_hasAttributes(node:PDomNode):gboolean;cdecl; external gtkhtmllib;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

  function DOM_TYPE_NODE : GType;
  begin
    DOM_TYPE_NODE:=dom_node_get_type;
  end;//DOM_TYPE_NODE

  function DOM_NODE(object : pointer) : PDomNode;
  begin
    DOM_NODE:=G_TYPE_CHECK_INSTANCE_CAST(object,DOM_TYPE_NODE,TDomNode);
  end;//DOM_NODE

  function DOM_NODE_CLASS(klass : pointer) : PDomNodeClass;
  begin
    DOM_NODE_CLASS:=G_TYPE_CHECK_CLASS_CAST(klass,DOM_TYPE_NODE,TDomNodeClass);
  end;//DOM_NODE_CLASS

  function DOM_IS_NODE(object : pointer) : boolean;
  begin
    DOM_IS_NODE:=G_TYPE_CHECK_INSTANCE_TYPE(object,DOM_TYPE_NODE);
  end;//DOM_IS_NODE

  function DOM_IS_NODE_CLASS(klass : pointer) : boolean;
  begin
    DOM_IS_NODE_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,DOM_TYPE_NODE);
  end;//DOM_IS_NODE_CLASS

  function DOM_NODE_GET_CLASS(obj : pointer) : PDomNodeClass;
  begin
    DOM_NODE_GET_CLASS:=G_TYPE_INSTANCE_GET_CLASS(obj,DOM_TYPE_NODE,TDomNodeClass);
  end;//DOM_NODE_GET_CLASS

{$ENDIF read_implementation}
// included by gtkhtml.pas
