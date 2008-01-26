// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ The GtkTextMark data type  }
   PGtkTextMark = ^TGtkTextMark;
   TGtkTextMark = record
        parent_instance : TGObject;
        segment : gpointer;
     end;

{ Padding for future expansion  }
   PGtkTextMarkClass = ^TGtkTextMarkClass;
   TGtkTextMarkClass = record
        parent_class : TGObjectClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_TEXT_MARK : GType;
function GTK_TEXT_MARK(anObject: pointer) : PGtkTextMark;
function GTK_TEXT_MARK_CLASS(klass: pointer) : PGtkTextMarkClass;
function GTK_IS_TEXT_MARK(anObject: pointer) : boolean;
function GTK_IS_TEXT_MARK_CLASS(klass: pointer) : boolean;
function GTK_TEXT_MARK_GET_CLASS(obj: pointer) : PGtkTextMarkClass;

function gtk_text_mark_get_type:GType; cdecl; external gtklib;
procedure gtk_text_mark_set_visible(mark:PGtkTextMark; setting:gboolean); cdecl; external gtklib;
function gtk_text_mark_get_visible(mark:PGtkTextMark):gboolean; cdecl; external gtklib;
function gtk_text_mark_get_name(mark:PGtkTextMark):Pgchar; cdecl; external gtklib;
function gtk_text_mark_get_deleted(mark:PGtkTextMark):gboolean; cdecl; external gtklib;
function gtk_text_mark_get_buffer(mark:PGtkTextMark):PGtkTextBuffer; cdecl; external gtklib;
function gtk_text_mark_get_left_gravity(mark:PGtkTextMark):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TEXT_MARK : GType;
begin
   GTK_TYPE_TEXT_MARK:=gtk_text_mark_get_type;
end;

function GTK_TEXT_MARK(anObject: pointer) : PGtkTextMark;
begin
   GTK_TEXT_MARK:=PGtkTextMark(G_TYPE_CHECK_INSTANCE_CAST(anObject,GTK_TYPE_TEXT_MARK));
end;

function GTK_TEXT_MARK_CLASS(klass: pointer) : PGtkTextMarkClass;
begin
   GTK_TEXT_MARK_CLASS:=PGtkTextMarkClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_TEXT_MARK));
end;

function GTK_IS_TEXT_MARK(anObject: pointer) : boolean;
begin
   GTK_IS_TEXT_MARK:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GTK_TYPE_TEXT_MARK);
end;

function GTK_IS_TEXT_MARK_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TEXT_MARK_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_TEXT_MARK);
end;

function GTK_TEXT_MARK_GET_CLASS(obj: pointer) : PGtkTextMarkClass;
begin
   GTK_TEXT_MARK_GET_CLASS:=PGtkTextMarkClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_TEXT_MARK));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

