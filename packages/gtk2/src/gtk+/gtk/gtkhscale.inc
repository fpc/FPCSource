// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkHScale = ^TGtkHScale;
   TGtkHScale = record
        scale : TGtkScale;
     end;

   PGtkHScaleClass = ^TGtkHScaleClass;
   TGtkHScaleClass = record
        parent_class : TGtkScaleClass;
     end;


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_HSCALE : GType;
function GTK_HSCALE(obj: pointer) : PGtkHScale;
function GTK_HSCALE_CLASS(klass: pointer) : PGtkHScaleClass;
function GTK_IS_HSCALE(obj: pointer) : boolean;
function GTK_IS_HSCALE_CLASS(klass: pointer) : boolean;
function GTK_HSCALE_GET_CLASS(obj: pointer) : PGtkHScaleClass;

function gtk_hscale_get_type:TGtkType; cdecl; external gtklib;
function gtk_hscale_new(adjustment:PGtkAdjustment):PGtkWidget; cdecl; external gtklib;
function gtk_hscale_new_with_range(min:gdouble; max:gdouble; step:gdouble):PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_HSCALE : GType;
begin
   GTK_TYPE_HSCALE:=gtk_hscale_get_type;
end;

function GTK_HSCALE(obj: pointer) : PGtkHScale;
begin
   GTK_HSCALE:=PGtkHScale(GTK_CHECK_CAST(obj,GTK_TYPE_HSCALE));
end;

function GTK_HSCALE_CLASS(klass: pointer) : PGtkHScaleClass;
begin
   GTK_HSCALE_CLASS:=PGtkHScaleClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_HSCALE));
end;

function GTK_IS_HSCALE(obj: pointer) : boolean;
begin
   GTK_IS_HSCALE:=GTK_CHECK_TYPE(obj,GTK_TYPE_HSCALE);
end;

function GTK_IS_HSCALE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_HSCALE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_HSCALE);
end;

function GTK_HSCALE_GET_CLASS(obj: pointer) : PGtkHScaleClass;
begin
   GTK_HSCALE_GET_CLASS:=PGtkHScaleClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_HSCALE));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

