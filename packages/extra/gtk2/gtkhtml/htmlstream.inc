// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

  PHtmlStream = ^THtmlStream;

  THtmlStreamCloseFunc = procedure (stream:PHtmlStream; user_data:gpointer);cdecl;
  THtmlStreamWriteFunc = procedure (stream:PHtmlStream; buffer:Pgchar; size:guint; user_data:gpointer);cdecl;
  THtmlStreamCancelFunc = procedure (stream:PHtmlStream; user_data:gpointer; cancel_data:gpointer);cdecl;

  THtmlStream = record
    parent_object : TGObject;
    write_func : THtmlStreamWriteFunc;
    close_func : THtmlStreamCloseFunc;
    cancel_func : THtmlStreamCancelFunc;
    user_data : gpointer;
    cancel_data : gpointer;
    written : gint;
    mime_type : pchar;
  end;//THtmlStream

  PHtmlStreamClass = ^THtmlStreamClass;
  THtmlStreamClass = record
    parent_class : TGObjectClass;
  end;//


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  function HTML_TYPE_STREAM : GType;
  function HTML_STREAM(obj : pointer) : PHtmlStream;
  function HTML_STREAM_CLASS(klass : pointer) : PHtmlStreamClass;
  function HTML_IS_STREAM(obj : pointer) : boolean;
  function HTML_IS_STREAM_CLASS(klass : pointer) : boolean;
  function HTML_STREAM_GET_CLASS(obj : pointer) : PHtmlStreamClass;

  function html_stream_get_type:GType;cdecl; external gtkhtmllib;
  function html_stream_new(write_func:THtmlStreamWriteFunc; close_func:THtmlStreamCloseFunc; user_data:gpointer):PHtmlStream;cdecl; external gtkhtmllib;
  procedure html_stream_write(stream:PHtmlStream; buffer:Pgchar; size:guint);cdecl; external gtkhtmllib;
  procedure html_stream_close(stream:PHtmlStream);cdecl; external gtkhtmllib;
  procedure html_stream_destroy(stream:PHtmlStream);cdecl; external gtkhtmllib;
  function html_stream_get_written(stream:PHtmlStream):gint;cdecl; external gtkhtmllib;
  procedure html_stream_cancel(stream:PHtmlStream);cdecl; external gtkhtmllib;
  procedure html_stream_set_cancel_func(stream:PHtmlStream; abort_func:THtmlStreamCancelFunc; cancel_data:gpointer);cdecl; external gtkhtmllib;
  function html_stream_get_mime_type(stream:PHtmlStream):pchar;cdecl; external gtkhtmllib;
  procedure html_stream_set_mime_type(stream:PHtmlStream; mime_type:Pchar);cdecl; external gtkhtmllib;


{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

  function HTML_TYPE_STREAM : GType;
  begin
    HTML_TYPE_STREAM:=html_stream_get_type;
  end;//HTML_TYPE_STREAM

  function HTML_STREAM(obj : pointer) : PHtmlStream;
  begin
    HTML_STREAM:=PHtmlStream(G_TYPE_CHECK_INSTANCE_CAST(obj,HTML_TYPE_STREAM));
  end;//HTML_STREAM

  function HTML_STREAM_CLASS(klass : pointer) : PHtmlStreamClass;
  begin
    HTML_STREAM_CLASS:=G_TYPE_CHECK_CLASS_CAST(klass,HTML_TYPE_STREAM);
  end;//HTML_STREAM_CLASS

  function HTML_IS_STREAM(obj : pointer) : boolean;
  begin
    HTML_IS_STREAM:=G_TYPE_CHECK_INSTANCE_TYPE(obj,HTML_TYPE_STREAM);
  end;//HTML_IS_STREAM

  function HTML_IS_STREAM_CLASS(klass : pointer) : boolean;
  begin
    HTML_IS_STREAM_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,HTML_TYPE_STREAM);
  end;//HTML_IS_STREAM_CLASS

  function HTML_STREAM_GET_CLASS(obj : pointer) : PHtmlStreamClass;
  begin
    HTML_STREAM_GET_CLASS:=PHtmlStreamClass(G_TYPE_INSTANCE_GET_CLASS(obj,HTML_TYPE_STREAM));
  end;//HTML_STREAM_GET_CLASS


{$ENDIF read_implementation}
// included by gtkhtml.pas
