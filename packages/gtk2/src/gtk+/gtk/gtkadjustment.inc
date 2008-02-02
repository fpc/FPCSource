// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkAdjustment = ^TGtkAdjustment;
   TGtkAdjustment = record
        parent_instance : TGtkObject;
        lower : gdouble;
        upper : gdouble;
        value : gdouble;
        step_increment : gdouble;
        page_increment : gdouble;
        page_size : gdouble;
     end;

{ Padding for future expansion  }
   PGtkAdjustmentClass = ^TGtkAdjustmentClass;
   TGtkAdjustmentClass = record
        parent_class : TGtkObjectClass;
        changed : procedure (adjustment:PGtkAdjustment); cdecl;
        value_changed : procedure (adjustment:PGtkAdjustment); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_ADJUSTMENT : GType;
function GTK_ADJUSTMENT(obj: pointer) : PGtkAdjustment;
function GTK_ADJUSTMENT_CLASS(klass: pointer) : PGtkAdjustmentClass;
function GTK_IS_ADJUSTMENT(obj: pointer) : boolean;
function GTK_IS_ADJUSTMENT_CLASS(klass: pointer) : boolean;
function GTK_ADJUSTMENT_GET_CLASS(obj: pointer) : PGtkAdjustmentClass;


function gtk_adjustment_get_type:TGtkType; cdecl; external gtklib;
function gtk_adjustment_new(value:gdouble; lower:gdouble; upper:gdouble; step_increment:gdouble; page_increment:gdouble;
           page_size:gdouble):PGtkObject; cdecl; external gtklib;
procedure gtk_adjustment_changed(adjustment:PGtkAdjustment); cdecl; external gtklib;
procedure gtk_adjustment_value_changed(adjustment:PGtkAdjustment); cdecl; external gtklib;
procedure gtk_adjustment_clamp_page(adjustment:PGtkAdjustment; lower:gdouble; upper:gdouble); cdecl; external gtklib;
function gtk_adjustment_get_value(adjustment:PGtkAdjustment):gdouble; cdecl; external gtklib;
procedure gtk_adjustment_set_value(adjustment:PGtkAdjustment; value:gdouble); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ADJUSTMENT : GType;
begin
   GTK_TYPE_ADJUSTMENT:=gtk_adjustment_get_type;
end;

function GTK_ADJUSTMENT(obj: pointer) : PGtkAdjustment;
begin
   GTK_ADJUSTMENT:=PGtkAdjustment(GTK_CHECK_CAST(obj,GTK_TYPE_ADJUSTMENT));
end;

function GTK_ADJUSTMENT_CLASS(klass: pointer) : PGtkAdjustmentClass;
begin
   GTK_ADJUSTMENT_CLASS:=PGtkAdjustmentClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_ADJUSTMENT));
end;

function GTK_IS_ADJUSTMENT(obj: pointer) : boolean;
begin
   GTK_IS_ADJUSTMENT:=GTK_CHECK_TYPE(obj,GTK_TYPE_ADJUSTMENT);
end;

function GTK_IS_ADJUSTMENT_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ADJUSTMENT_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_ADJUSTMENT);
end;

function GTK_ADJUSTMENT_GET_CLASS(obj: pointer) : PGtkAdjustmentClass;
begin
   GTK_ADJUSTMENT_GET_CLASS:=PGtkAdjustmentClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_ADJUSTMENT));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

