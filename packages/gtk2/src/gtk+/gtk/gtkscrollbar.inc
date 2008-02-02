// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkScrollbar = ^TGtkScrollbar;
   TGtkScrollbar = record
        range : TGtkRange;
     end;

{ Padding for future expansion  }
   PGtkScrollbarClass = ^TGtkScrollbarClass;
   TGtkScrollbarClass = record
        parent_class : TGtkRangeClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_SCROLLBAR : GType;
function GTK_SCROLLBAR(obj: pointer) : PGtkScrollbar;
function GTK_SCROLLBAR_CLASS(klass: pointer) : PGtkScrollbarClass;
function GTK_IS_SCROLLBAR(obj: pointer) : boolean;
function GTK_IS_SCROLLBAR_CLASS(klass: pointer) : boolean;
function GTK_SCROLLBAR_GET_CLASS(obj: pointer) : PGtkScrollbarClass;


function gtk_scrollbar_get_type:TGtkType; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_SCROLLBAR : GType;
begin
   GTK_TYPE_SCROLLBAR:=gtk_scrollbar_get_type;
end;

function GTK_SCROLLBAR(obj: pointer) : PGtkScrollbar;
begin
   GTK_SCROLLBAR:=PGtkScrollbar(GTK_CHECK_CAST(obj,GTK_TYPE_SCROLLBAR));
end;

function GTK_SCROLLBAR_CLASS(klass: pointer) : PGtkScrollbarClass;
begin
   GTK_SCROLLBAR_CLASS:=PGtkScrollbarClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_SCROLLBAR));
end;

function GTK_IS_SCROLLBAR(obj: pointer) : boolean;
begin
   GTK_IS_SCROLLBAR:=GTK_CHECK_TYPE(obj,GTK_TYPE_SCROLLBAR);
end;

function GTK_IS_SCROLLBAR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_SCROLLBAR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_SCROLLBAR);
end;

function GTK_SCROLLBAR_GET_CLASS(obj: pointer) : PGtkScrollbarClass;
begin
   GTK_SCROLLBAR_GET_CLASS:=PGtkScrollbarClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_SCROLLBAR));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

