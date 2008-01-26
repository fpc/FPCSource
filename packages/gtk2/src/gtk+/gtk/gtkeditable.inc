// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkEditable = pointer;

{ signals  }
{ vtable  }
   PGtkEditableClass = ^TGtkEditableClass;
   TGtkEditableClass = record
        base_iface : TGTypeInterface;
        insert_text : procedure (editable:PGtkEditable; text:Pgchar; length:gint; position:Pgint); cdecl;
        delete_text : procedure (editable:PGtkEditable; start_pos:gint; end_pos:gint); cdecl;
        changed : procedure (editable:PGtkEditable); cdecl;
        do_insert_text : procedure (editable:PGtkEditable; text:Pgchar; length:gint; position:Pgint); cdecl;
        do_delete_text : procedure (editable:PGtkEditable; start_pos:gint; end_pos:gint); cdecl;
        get_chars : function (editable:PGtkEditable; start_pos:gint; end_pos:gint):Pgchar; cdecl;
        set_selection_bounds : procedure (editable:PGtkEditable; start_pos:gint; end_pos:gint); cdecl;
        get_selection_bounds : function (editable:PGtkEditable; start_pos:Pgint; end_pos:Pgint):gboolean; cdecl;
        set_position : procedure (editable:PGtkEditable; position:gint); cdecl;
        get_position : function (editable:PGtkEditable):gint; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_EDITABLE : GType;
function GTK_EDITABLE(obj: pointer) : PGtkEditable;
function GTK_EDITABLE_CLASS(vtable : pointer) : PGtkEditableClass;
function GTK_IS_EDITABLE(obj: pointer) : boolean;
function GTK_IS_EDITABLE_CLASS(vtable : pointer) : boolean;
function GTK_EDITABLE_GET_CLASS(inst : pointer) : PGtkEditableClass;


function gtk_editable_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_editable_select_region(editable:PGtkEditable; start:gint; theEnd:gint); cdecl; external gtklib;
function gtk_editable_get_selection_bounds(editable:PGtkEditable; start:Pgint; theEnd:Pgint):gboolean; cdecl; external gtklib;
procedure gtk_editable_insert_text(editable:PGtkEditable; new_text:Pgchar; new_text_length:gint; position:Pgint); cdecl; external gtklib;
procedure gtk_editable_delete_text(editable:PGtkEditable; start_pos:gint; end_pos:gint); cdecl; external gtklib;
function gtk_editable_get_chars(editable:PGtkEditable; start_pos:gint; end_pos:gint):Pgchar; cdecl; external gtklib;
procedure gtk_editable_cut_clipboard(editable:PGtkEditable); cdecl; external gtklib;
procedure gtk_editable_copy_clipboard(editable:PGtkEditable); cdecl; external gtklib;
procedure gtk_editable_paste_clipboard(editable:PGtkEditable); cdecl; external gtklib;
procedure gtk_editable_delete_selection(editable:PGtkEditable); cdecl; external gtklib;
procedure gtk_editable_set_position(editable:PGtkEditable; position:gint); cdecl; external gtklib;
function gtk_editable_get_position(editable:PGtkEditable):gint; cdecl; external gtklib;
procedure gtk_editable_set_editable(editable:PGtkEditable; is_editable:gboolean); cdecl; external gtklib;
function gtk_editable_get_editable(editable:PGtkEditable):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_EDITABLE : GType;
begin
   GTK_TYPE_EDITABLE:=gtk_editable_get_type;
end;

function GTK_EDITABLE(obj: pointer) : PGtkEditable;
begin
   GTK_EDITABLE:=PGtkEditable(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_EDITABLE));
end;

function GTK_EDITABLE_CLASS(vtable : pointer) : PGtkEditableClass;
begin
   GTK_EDITABLE_CLASS:=PGtkEditableClass(G_TYPE_CHECK_CLASS_CAST(vtable,GTK_TYPE_EDITABLE));
end;

function GTK_IS_EDITABLE(obj: pointer) : boolean;
begin
   GTK_IS_EDITABLE:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_EDITABLE);
end;

function GTK_IS_EDITABLE_CLASS(vtable : pointer) : boolean;
begin
   GTK_IS_EDITABLE_CLASS:=G_TYPE_CHECK_CLASS_TYPE(vtable,GTK_TYPE_EDITABLE);
end;

function GTK_EDITABLE_GET_CLASS(inst : pointer) : PGtkEditableClass;
begin
   GTK_EDITABLE_GET_CLASS:=PGtkEditableClass(G_TYPE_INSTANCE_GET_INTERFACE(inst,GTK_TYPE_EDITABLE));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

