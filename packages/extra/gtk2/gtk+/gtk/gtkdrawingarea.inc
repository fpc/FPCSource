// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkDrawingArea = ^TGtkDrawingArea;
   TGtkDrawingArea = record
        widget : TGtkWidget;
        draw_data : gpointer;
     end;

{ Padding for future expansion  }
   PGtkDrawingAreaClass = ^TGtkDrawingAreaClass;
   TGtkDrawingAreaClass = record
        parent_class : TGtkWidgetClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_DRAWING_AREA : GType;
function GTK_DRAWING_AREA(obj: pointer) : PGtkDrawingArea;
function GTK_DRAWING_AREA_CLASS(klass: pointer) : PGtkDrawingAreaClass;
function GTK_IS_DRAWING_AREA(obj: pointer) : boolean;
function GTK_IS_DRAWING_AREA_CLASS(klass: pointer) : boolean;
function GTK_DRAWING_AREA_GET_CLASS(obj: pointer) : PGtkDrawingAreaClass;


function gtk_drawing_area_get_type:TGtkType; cdecl; external gtklib;
function gtk_drawing_area_new:PGtkWidget; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
procedure gtk_drawing_area_size(darea:PGtkDrawingArea; width:gint; height:gint); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_DRAWING_AREA : GType;
begin
   GTK_TYPE_DRAWING_AREA:=gtk_drawing_area_get_type;
end;

function GTK_DRAWING_AREA(obj: pointer) : PGtkDrawingArea;
begin
   GTK_DRAWING_AREA:=PGtkDrawingArea(GTK_CHECK_CAST(obj,GTK_TYPE_DRAWING_AREA));
end;

function GTK_DRAWING_AREA_CLASS(klass: pointer) : PGtkDrawingAreaClass;
begin
   GTK_DRAWING_AREA_CLASS:=PGtkDrawingAreaClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_DRAWING_AREA));
end;

function GTK_IS_DRAWING_AREA(obj: pointer) : boolean;
begin
   GTK_IS_DRAWING_AREA:=GTK_CHECK_TYPE(obj,GTK_TYPE_DRAWING_AREA);
end;

function GTK_IS_DRAWING_AREA_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_DRAWING_AREA_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_DRAWING_AREA);
end;

function GTK_DRAWING_AREA_GET_CLASS(obj: pointer) : PGtkDrawingAreaClass;
begin
   GTK_DRAWING_AREA_GET_CLASS:=PGtkDrawingAreaClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_DRAWING_AREA));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

