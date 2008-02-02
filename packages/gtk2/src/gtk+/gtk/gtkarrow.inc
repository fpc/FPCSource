// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkArrow = ^TGtkArrow;
   TGtkArrow = record
        misc : TGtkMisc;
        arrow_type : gint16;
        shadow_type : gint16;
     end;

   PGtkArrowClass = ^TGtkArrowClass;
   TGtkArrowClass = record
        parent_class : TGtkMiscClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_ARROW : GType;
function GTK_ARROW(obj: pointer) : PGtkArrow;
function GTK_ARROW_CLASS(klass: pointer) : PGtkArrowClass;
function GTK_IS_ARROW(obj: pointer) : boolean;
function GTK_IS_ARROW_CLASS(klass: pointer) : boolean;
function GTK_ARROW_GET_CLASS(obj: pointer) : PGtkArrowClass;


function gtk_arrow_get_type:TGtkType; cdecl; external gtklib;
function gtk_arrow_new(arrow_type:TGtkArrowType; shadow_type:TGtkShadowType):PGtkWidget; cdecl; external gtklib;
procedure gtk_arrow_set(arrow:PGtkArrow; arrow_type:TGtkArrowType; shadow_type:TGtkShadowType); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ARROW : GType;
begin
   GTK_TYPE_ARROW:=gtk_arrow_get_type;
end;

function GTK_ARROW(obj: pointer) : PGtkArrow;
begin
   GTK_ARROW:=PGtkArrow(GTK_CHECK_CAST(obj,GTK_TYPE_ARROW));
end;

function GTK_ARROW_CLASS(klass: pointer) : PGtkArrowClass;
begin
   GTK_ARROW_CLASS:=PGtkArrowClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_ARROW));
end;

function GTK_IS_ARROW(obj: pointer) : boolean;
begin
   GTK_IS_ARROW:=GTK_CHECK_TYPE(obj,GTK_TYPE_ARROW);
end;

function GTK_IS_ARROW_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ARROW_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_ARROW);
end;

function GTK_ARROW_GET_CLASS(obj: pointer) : PGtkArrowClass;
begin
   GTK_ARROW_GET_CLASS:=PGtkArrowClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_ARROW));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

