// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

  PHtmlFocusIterator = ^THtmlFocusIterator;
  THtmlFocusIterator = record
    parent_instance : TGObject;
    document : PDomDocument;
    current_node : PDomNode;
  end;//THtmlFocusIterator

  PHtmlFocusIteratorClass = ^THtmlFocusIteratorClass;
  THtmlFocusIteratorClass = record
    parent_class : TGObjectClass;
  end;//THtmlFocusIteratorClass

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  function HTML_TYPE_FOCUS_ITERATOR : GType;
  function HTML_FOCUS_ITERATOR(object : pointer) : PHtmlFocusIterator;
  function HTML_FOCUS_ITERATOR_CLASS(klass : pointer) : PHtmlFocusIteratorClass;
  function HTML_IS_FOCUS_ITERATOR(object : pointer) : boolean;
  function HTML_IS_FOCUS_ITERATOR_CLASS(klass : pointer) : boolean;
  function HTML_FOCUS_ITERATOR_GET_CLASS(obj : pointer) : PHtmlFocusIteratorClass;

  function html_focus_iterator_next_element(document:PDomDocument; element:PDomElement):PDomElement;cdecl; external gtkhtmllib;
  function html_focus_iterator_prev_element(document:PDomDocument; element:PDomElement):PDomElement;cdecl; external gtkhtmllib;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

  function HTML_TYPE_FOCUS_ITERATOR : GType;
  begin
    HTML_TYPE_FOCUS_ITERATOR:=html_focus_iterator_get_type;
  end;//HTML_TYPE_FOCUS_ITERATOR

  function HTML_FOCUS_ITERATOR(object : pointer) : PHtmlFocusIterator;
  begin
    HTML_FOCUS_ITERATOR:=G_TYPE_CHECK_INSTANCE_CAST(object,HTML_TYPE_FOCUS_ITERATOR,HtmlFocusIterator);
  end;//HTML_FOCUS_ITERATOR

  function HTML_FOCUS_ITERATOR_CLASS(klass : pointer) : PHtmlFocusIteratorClass;
  begin
    HTML_FOCUS_ITERATOR_CLASS:=G_TYPE_CHECK_CLASS_CAST(klass,HTML_TYPE_FOCUS_ITERATOR,HtmlFocusIteratorClass);
  end;//HTML_FOCUS_ITERATOR_CLASS

  function HTML_IS_FOCUS_ITERATOR(object : pointer) : boolean;
  begin
    HTML_IS_FOCUS_ITERATOR:=G_TYPE_CHECK_INSTANCE_TYPE(object,HTML_TYPE_FOCUS_ITERATOR);
  end;//HTML_IS_FOCUS_ITERATOR

  function HTML_IS_FOCUS_ITERATOR_CLASS(klass : pointer) : boolean;
  begin
    HTML_IS_FOCUS_ITERATOR_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,HTML_TYPE_FOCUS_ITERATOR);
  end;//HTML_IS_FOCUS_ITERATOR_CLASS

  function HTML_FOCUS_ITERATOR_GET_CLASS(obj : pointer) : PHtmlFocusIteratorClass;
  begin
    HTML_FOCUS_ITERATOR_GET_CLASS:=G_TYPE_INSTANCE_GET_CLASS(obj,HTML_TYPE_FOCUS_ITERATOR,HtmlFocusIteratorClass);
  end;//HTML_FOCUS_ITERATOR_GET_CLASS

{$ENDIF read_implementation}
// included by gtkhtml.pas
