// included by atk.pp

{
   AtkEditableText is used to support access in an "accessibility" context
   to editing features of editable text widgets.
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PAtkEditableTextIface = ^TAtkEditableTextIface;
   TAtkEditableTextIface = record
        parent_interface : TGTypeInterface;
        set_run_attributes : function (text:PAtkEditableText; attrib_set:PAtkAttributeSet; start_offset:gint; end_offset:gint):gboolean; cdecl;
        set_text_contents : procedure (text:PAtkEditableText; _string:Pgchar); cdecl;
        insert_text : procedure (text:PAtkEditableText; _string:Pgchar; length:gint; position:Pgint); cdecl;
        copy_text : procedure (text:PAtkEditableText; start_pos:gint; end_pos:gint); cdecl;
        cut_text : procedure (text:PAtkEditableText; start_pos:gint; end_pos:gint); cdecl;
        delete_text : procedure (text:PAtkEditableText; start_pos:gint; end_pos:gint); cdecl;
        paste_text : procedure (text:PAtkEditableText; position:gint); cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_EDITABLE_TEXT : GType;
function ATK_IS_EDITABLE_TEXT(obj: pointer) : boolean;
function ATK_EDITABLE_TEXT(obj: pointer) : PAtkEditableText;
function ATK_EDITABLE_TEXT_GET_IFACE(obj: pointer) : PAtkEditableTextIface;


function atk_editable_text_get_type:GType; cdecl; external atklib;
function atk_editable_text_set_run_attributes(text:PAtkEditableText; attrib_set:PAtkAttributeSet; start_offset:gint; end_offset:gint):gboolean; cdecl; external atklib;
procedure atk_editable_text_set_text_contents(text:PAtkEditableText; _string:Pgchar); cdecl; external atklib;
procedure atk_editable_text_insert_text(text:PAtkEditableText; _string:Pgchar; length:gint; position:Pgint); cdecl; external atklib;
procedure atk_editable_text_copy_text(text:PAtkEditableText; start_pos:gint; end_pos:gint); cdecl; external atklib;
procedure atk_editable_text_cut_text(text:PAtkEditableText; start_pos:gint; end_pos:gint); cdecl; external atklib;
procedure atk_editable_text_delete_text(text:PAtkEditableText; start_pos:gint; end_pos:gint); cdecl; external atklib;
procedure atk_editable_text_paste_text(text:PAtkEditableText; position:gint); cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_EDITABLE_TEXT : GType;
begin
   ATK_TYPE_EDITABLE_TEXT:=atk_editable_text_get_type;
end;

function ATK_IS_EDITABLE_TEXT(obj: pointer) : boolean;
begin
   ATK_IS_EDITABLE_TEXT:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_EDITABLE_TEXT);
end;

function ATK_EDITABLE_TEXT(obj: pointer) : PAtkEditableText;
begin
   ATK_EDITABLE_TEXT:=PAtkEditableText(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_EDITABLE_TEXT));
end;

function ATK_EDITABLE_TEXT_GET_IFACE(obj: pointer) : PAtkEditableTextIface;
begin
   ATK_EDITABLE_TEXT_GET_IFACE:=PAtkEditableTextIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_EDITABLE_TEXT));
end;

{$ENDIF read_implementation}

