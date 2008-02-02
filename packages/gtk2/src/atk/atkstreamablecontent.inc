// included by atk.pp

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{  Get the number of mime types supported by this anObject }
{  Gets the specified mime type supported by this anObject.
   The mime types are 0-based so the first mime type is
   at index 0, the second at index 1 and so on.

   This assumes that the strings for the mime types are stored in the
   AtkStreamableContent. Alternatively the  could be removed
   and the caller would be responsible for calling g_free() on the
   returned value.
    }
{
   Is one possible implementation for this method that it constructs the
   content appropriate for the mime type and then creates a temporary
   file containing the content, opens the file and then calls
   g_io_channel_unix_new_fd().
    }
   PAtkStreamableContentIface = ^TAtkStreamableContentIface;
   TAtkStreamableContentIface = record
        parent : TGTypeInterface;
        get_n_mime_types : function (streamable:PAtkStreamableContent):gint; cdecl;
        get_mime_type : function (streamable:PAtkStreamableContent; i:gint):Pgchar; cdecl;
        get_stream : function (streamable:PAtkStreamableContent; mime_type:Pgchar):PGIOChannel; cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
        pad3 : TAtkFunction;
        pad4 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_STREAMABLE_CONTENT : GType;
function ATK_IS_STREAMABLE_CONTENT(obj: pointer) : boolean;
function ATK_STREAMABLE_CONTENT(obj: pointer) : PAtkStreamableContent;
function ATK_STREAMABLE_CONTENT_GET_IFACE(obj: pointer) : PAtkStreamableContentIface;


function atk_streamable_content_get_type:GType; cdecl; external atklib;
function atk_streamable_content_get_n_mime_types(streamable:PAtkStreamableContent):gint; cdecl; external atklib;
function atk_streamable_content_get_mime_type(streamable:PAtkStreamableContent; i:gint):Pgchar; cdecl; external atklib;
function atk_streamable_content_get_stream(streamable:PAtkStreamableContent; mime_type:Pgchar):PGIOChannel; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_STREAMABLE_CONTENT : GType;
begin
   ATK_TYPE_STREAMABLE_CONTENT:=atk_streamable_content_get_type;
end;

function ATK_IS_STREAMABLE_CONTENT(obj: pointer) : boolean;
begin
   ATK_IS_STREAMABLE_CONTENT:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_STREAMABLE_CONTENT);
end;

function ATK_STREAMABLE_CONTENT(obj: pointer) : PAtkStreamableContent;
begin
   ATK_STREAMABLE_CONTENT:=PAtkStreamableContent(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_STREAMABLE_CONTENT));
end;

function ATK_STREAMABLE_CONTENT_GET_IFACE(obj: pointer) : PAtkStreamableContentIface;
begin
   ATK_STREAMABLE_CONTENT_GET_IFACE:=PAtkStreamableContentIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_STREAMABLE_CONTENT));
end;

{$ENDIF read_implementation}

