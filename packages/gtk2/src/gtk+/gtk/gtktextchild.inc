// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ A GtkTextChildAnchor is a spot in the buffer where child widgets
   can be "anchored" (inserted inline, as if they were characters).
   The anchor can have multiple widgets anchored, to allow for multiple
   views.
  }
   PGtkTextChildAnchor = ^TGtkTextChildAnchor;
   TGtkTextChildAnchor = record
        parent_instance : TGObject;
        segment : gpointer;
     end;

{ Padding for future expansion  }
   PGtkTextChildAnchorClass = ^TGtkTextChildAnchorClass;
   TGtkTextChildAnchorClass = record
        parent_class : TGObjectClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_TEXT_CHILD_ANCHOR : GType;
function GTK_TEXT_CHILD_ANCHOR(anObject: pointer) : PGtkTextChildAnchor;
function GTK_TEXT_CHILD_ANCHOR_CLASS(klass: pointer) : PGtkTextChildAnchorClass;
function GTK_IS_TEXT_CHILD_ANCHOR(anObject: pointer) : boolean;
function GTK_IS_TEXT_CHILD_ANCHOR_CLASS(klass: pointer) : boolean;
function GTK_TEXT_CHILD_ANCHOR_GET_CLASS(obj: pointer) : PGtkTextChildAnchorClass;


function gtk_text_child_anchor_get_type:GType; cdecl; external gtklib;
function gtk_text_child_anchor_new:PGtkTextChildAnchor; cdecl; external gtklib;
function gtk_text_child_anchor_get_widgets(anchor:PGtkTextChildAnchor):PGList; cdecl; external gtklib;
function gtk_text_child_anchor_get_deleted(anchor:PGtkTextChildAnchor):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TEXT_CHILD_ANCHOR : GType;
begin
   GTK_TYPE_TEXT_CHILD_ANCHOR:=gtk_text_child_anchor_get_type;
end;

function GTK_TEXT_CHILD_ANCHOR(anObject: pointer) : PGtkTextChildAnchor;
begin
   GTK_TEXT_CHILD_ANCHOR:=PGtkTextChildAnchor(G_TYPE_CHECK_INSTANCE_CAST(anObject,GTK_TYPE_TEXT_CHILD_ANCHOR));
end;

function GTK_TEXT_CHILD_ANCHOR_CLASS(klass: pointer) : PGtkTextChildAnchorClass;
begin
   GTK_TEXT_CHILD_ANCHOR_CLASS:=PGtkTextChildAnchorClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_TEXT_CHILD_ANCHOR));
end;

function GTK_IS_TEXT_CHILD_ANCHOR(anObject: pointer) : boolean;
begin
   GTK_IS_TEXT_CHILD_ANCHOR:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GTK_TYPE_TEXT_CHILD_ANCHOR);
end;

function GTK_IS_TEXT_CHILD_ANCHOR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TEXT_CHILD_ANCHOR_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_TEXT_CHILD_ANCHOR);
end;

function GTK_TEXT_CHILD_ANCHOR_GET_CLASS(obj: pointer) : PGtkTextChildAnchorClass;
begin
   GTK_TEXT_CHILD_ANCHOR_GET_CLASS:=PGtkTextChildAnchorClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_TEXT_CHILD_ANCHOR));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

