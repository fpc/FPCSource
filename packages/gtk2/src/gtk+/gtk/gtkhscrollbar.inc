// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkHScrollbar = ^TGtkHScrollbar;
   TGtkHScrollbar = record
        scrollbar : TGtkScrollbar;
     end;

   PGtkHScrollbarClass = ^TGtkHScrollbarClass;
   TGtkHScrollbarClass = record
        parent_class : TGtkScrollbarClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_HSCROLLBAR : GType;
function GTK_HSCROLLBAR(obj: pointer) : PGtkHScrollbar;
function GTK_HSCROLLBAR_CLASS(klass: pointer) : PGtkHScrollbarClass;
function GTK_IS_HSCROLLBAR(obj: pointer) : boolean;
function GTK_IS_HSCROLLBAR_CLASS(klass: pointer) : boolean;
function GTK_HSCROLLBAR_GET_CLASS(obj: pointer) : PGtkHScrollbarClass;


function gtk_hscrollbar_get_type:TGtkType; cdecl; external gtklib;
function gtk_hscrollbar_new(adjustment:PGtkAdjustment):PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_HSCROLLBAR : GType;
begin
   GTK_TYPE_HSCROLLBAR:=gtk_hscrollbar_get_type;
end;

function GTK_HSCROLLBAR(obj: pointer) : PGtkHScrollbar;
begin
   GTK_HSCROLLBAR:=PGtkHScrollbar(GTK_CHECK_CAST(obj,GTK_TYPE_HSCROLLBAR));
end;

function GTK_HSCROLLBAR_CLASS(klass: pointer) : PGtkHScrollbarClass;
begin
   GTK_HSCROLLBAR_CLASS:=PGtkHScrollbarClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_HSCROLLBAR));
end;

function GTK_IS_HSCROLLBAR(obj: pointer) : boolean;
begin
   GTK_IS_HSCROLLBAR:=GTK_CHECK_TYPE(obj,GTK_TYPE_HSCROLLBAR);
end;

function GTK_IS_HSCROLLBAR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_HSCROLLBAR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_HSCROLLBAR);
end;

function GTK_HSCROLLBAR_GET_CLASS(obj: pointer) : PGtkHScrollbarClass;
begin
   GTK_HSCROLLBAR_GET_CLASS:=PGtkHScrollbarClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_HSCROLLBAR));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

