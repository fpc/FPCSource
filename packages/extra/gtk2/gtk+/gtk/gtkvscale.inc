// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkVScale = ^TGtkVScale;
   TGtkVScale = record
        scale : TGtkScale;
     end;

   PGtkVScaleClass = ^TGtkVScaleClass;
   TGtkVScaleClass = record
        parent_class : TGtkScaleClass;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_VSCALE : GType;
function GTK_VSCALE(obj: pointer) : PGtkVScale;
function GTK_VSCALE_CLASS(klass: pointer) : PGtkVScaleClass;
function GTK_IS_VSCALE(obj: pointer) : boolean;
function GTK_IS_VSCALE_CLASS(klass: pointer) : boolean;
function GTK_VSCALE_GET_CLASS(obj: pointer) : PGtkVScaleClass;



function gtk_vscale_get_type:TGtkType; cdecl; external gtklib;
function gtk_vscale_new(adjustment:PGtkAdjustment):PGtkWidget; cdecl; external gtklib;
function gtk_vscale_new_with_range(min:gdouble; max:gdouble; step:gdouble):PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_VSCALE : GType;
begin
   GTK_TYPE_VSCALE:=gtk_vscale_get_type;
end;

function GTK_VSCALE(obj: pointer) : PGtkVScale;
begin
   GTK_VSCALE:=PGtkVScale(GTK_CHECK_CAST(obj,GTK_TYPE_VSCALE));
end;

function GTK_VSCALE_CLASS(klass: pointer) : PGtkVScaleClass;
begin
   GTK_VSCALE_CLASS:=PGtkVScaleClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_VSCALE));
end;

function GTK_IS_VSCALE(obj: pointer) : boolean;
begin
   GTK_IS_VSCALE:=GTK_CHECK_TYPE(obj,GTK_TYPE_VSCALE);
end;

function GTK_IS_VSCALE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_VSCALE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_VSCALE);
end;

function GTK_VSCALE_GET_CLASS(obj: pointer) : PGtkVScaleClass;
begin
   GTK_VSCALE_GET_CLASS:=PGtkVScaleClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_VSCALE));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

