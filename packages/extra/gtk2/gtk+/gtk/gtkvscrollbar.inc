// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkVScrollbar = ^TGtkVScrollbar;
   TGtkVScrollbar = record
        scrollbar : TGtkScrollbar;
     end;

   PGtkVScrollbarClass = ^TGtkVScrollbarClass;
   TGtkVScrollbarClass = record
        parent_class : TGtkScrollbarClass;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_VSCROLLBAR : GType;
function GTK_VSCROLLBAR(obj: pointer) : PGtkVScrollbar;
function GTK_VSCROLLBAR_CLASS(klass: pointer) : PGtkVScrollbarClass;
function GTK_IS_VSCROLLBAR(obj: pointer) : boolean;
function GTK_IS_VSCROLLBAR_CLASS(klass: pointer) : boolean;
function GTK_VSCROLLBAR_GET_CLASS(obj: pointer) : PGtkVScrollbarClass;


function gtk_vscrollbar_get_type:TGtkType; cdecl; external gtklib;
function gtk_vscrollbar_new(adjustment:PGtkAdjustment):PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_VSCROLLBAR : GType;
begin
   GTK_TYPE_VSCROLLBAR:=gtk_vscrollbar_get_type;
end;

function GTK_VSCROLLBAR(obj: pointer) : PGtkVScrollbar;
begin
   GTK_VSCROLLBAR:=PGtkVScrollbar(GTK_CHECK_CAST(obj,GTK_TYPE_VSCROLLBAR));
end;

function GTK_VSCROLLBAR_CLASS(klass: pointer) : PGtkVScrollbarClass;
begin
   GTK_VSCROLLBAR_CLASS:=PGtkVScrollbarClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_VSCROLLBAR));
end;

function GTK_IS_VSCROLLBAR(obj: pointer) : boolean;
begin
   GTK_IS_VSCROLLBAR:=GTK_CHECK_TYPE(obj,GTK_TYPE_VSCROLLBAR);
end;

function GTK_IS_VSCROLLBAR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_VSCROLLBAR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_VSCROLLBAR);
end;

function GTK_VSCROLLBAR_GET_CLASS(obj: pointer) : PGtkVScrollbarClass;
begin
   GTK_VSCROLLBAR_GET_CLASS:=PGtkVScrollbarClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_VSCROLLBAR));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

