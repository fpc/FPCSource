// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

  PDomDocument = ^TDomDocument;
  TDomDocument = record
    parent : PDomNode;
    iterators : PGSList;
  end;//TDomDocument

  PDomDocumentClass = ^TDomDocumentClass;
  TDomDocumentClass = record
    parent_class : PDomNodeClass;
  end;//TDomDocumentClass

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  function DOM_TYPE_DOCUMENT : GType;
  function DOM_DOCUMENT(object : pointer) : PDomDocument;
  function DOM_DOCUMENT_CLASS(klass : pointer) : PDomDocumentClass;
  function DOM_IS_DOCUMENT(object : pointer) : boolean;
  function DOM_IS_DOCUMENT_CLASS(klass : pointer) : boolean;
  function DOM_DOCUMENT_GET_CLASS(obj : pointer) : PDomDocumentClass;

  function dom_document_get_type:GType;
  function dom_Document__get_documentElement(doc:PDomDocument):PDomElement;
  function dom_Document_createElement(doc:PDomDocument; tagName:PDomString):PDomElement;
  function dom_Document_createTextNode(doc:PDomDocument; data:PDomString):PDomText;
  function dom_Document_createComment(doc:PDomDocument; data:PDomString):PDomComment;
  function dom_Document_importNode(doc:PDomDocument; importedNode:PDomNode; deep:DomBoolean; exc:PDomException):PDomNode;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

  function DOM_TYPE_DOCUMENT : GType;
  begin
    DOM_TYPE_DOCUMENT:=dom_document_get_type;
  end;//DOM_TYPE_DOCUMENT

  function DOM_DOCUMENT(object : pointer) : PDomDocument;
  begin
    DOM_DOCUMENT:=G_TYPE_CHECK_INSTANCE_CAST(object,DOM_TYPE_DOCUMENT,TDomDocument);
  end;//DOM_DOCUMENT

  function DOM_DOCUMENT_CLASS(klass : pointer) : PDomDocumentClass;
  begin
    DOM_DOCUMENT_CLASS:=G_TYPE_CHECK_CLASS_CAST(klass,DOM_TYPE_DOCUMENT,TDomDocumentClass);
  end;//DOM_DOCUMENT_CLASS

  function DOM_IS_DOCUMENT(object : pointer) : boolean;
  begin
    DOM_IS_DOCUMENT:=G_TYPE_CHECK_INSTANCE_TYPE(object,DOM_TYPE_DOCUMENT);
  end;//DOM_IS_DOCUMENT

  function DOM_IS_DOCUMENT_CLASS(klass : pointer) : boolean;
  begin
    DOM_IS_DOCUMENT_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,DOM_TYPE_DOCUMENT);
  end;//DOM_IS_DOCUMENT_CLASS

  function DOM_DOCUMENT_GET_CLASS(obj : pointer) : PDomDocumentClass;
  begin
    DOM_DOCUMENT_GET_CLASS:=G_TYPE_INSTANCE_GET_CLASS(obj,DOM_TYPE_DOCUMENT,TDomDocumentClass);
  end;//DOM_DOCUMENT_GET_CLASS

{$ENDIF read_implementation}
// included by gtkhtml.pas
