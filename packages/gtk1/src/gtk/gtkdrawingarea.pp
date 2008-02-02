{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkDrawingArea = ^TGtkDrawingArea;
     TGtkDrawingArea = record
          widget : TGtkWidget;
          draw_data : gpointer;
       end;

     PGtkDrawingAreaClass = ^TGtkDrawingAreaClass;
     TGtkDrawingAreaClass = record
          parent_class : TGtkWidgetClass;
       end;

Type
  GTK_DRAWING_AREA=PGtkDrawingArea;
  GTK_DRAWING_AREA_CLASS=PGtkDrawingAreaClass;

function  GTK_DRAWING_AREA_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_drawing_area_get_type';
function  GTK_IS_DRAWING_AREA(obj:pointer):boolean;
function  GTK_IS_DRAWING_AREA_CLASS(klass:pointer):boolean;

function  gtk_drawing_area_get_type:TGtkType;cdecl;external gtkdll name 'gtk_drawing_area_get_type';
function  gtk_drawing_area_new : PGtkWidget;cdecl;external gtkdll name 'gtk_drawing_area_new';
procedure gtk_drawing_area_size(darea:PGtkDrawingArea; width:gint; height:gint);cdecl;external gtkdll name 'gtk_drawing_area_size';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_DRAWING_AREA(obj:pointer):boolean;
begin
  GTK_IS_DRAWING_AREA:=(obj<>nil) and GTK_IS_DRAWING_AREA_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_DRAWING_AREA_CLASS(klass:pointer):boolean;
begin
  GTK_IS_DRAWING_AREA_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_DRAWING_AREA_TYPE);
end;

{$endif read_implementation}


