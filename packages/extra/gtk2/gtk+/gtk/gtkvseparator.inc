// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkVSeparator = ^TGtkVSeparator;
   TGtkVSeparator = record
        separator : TGtkSeparator;
     end;

   PGtkVSeparatorClass = ^TGtkVSeparatorClass;
   TGtkVSeparatorClass = record
        parent_class : TGtkSeparatorClass;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_VSEPARATOR : GType;
function GTK_VSEPARATOR(obj: pointer) : PGtkVSeparator;
function GTK_VSEPARATOR_CLASS(klass: pointer) : PGtkVSeparatorClass;
function GTK_IS_VSEPARATOR(obj: pointer) : boolean;
function GTK_IS_VSEPARATOR_CLASS(klass: pointer) : boolean;
function GTK_VSEPARATOR_GET_CLASS(obj: pointer) : PGtkVSeparatorClass;



function gtk_vseparator_get_type:TGtkType; cdecl; external gtklib;
function gtk_vseparator_new:PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_VSEPARATOR : GType;
begin
   GTK_TYPE_VSEPARATOR:=gtk_vseparator_get_type;
end;

function GTK_VSEPARATOR(obj: pointer) : PGtkVSeparator;
begin
   GTK_VSEPARATOR:=PGtkVSeparator(GTK_CHECK_CAST(obj,GTK_TYPE_VSEPARATOR));
end;

function GTK_VSEPARATOR_CLASS(klass: pointer) : PGtkVSeparatorClass;
begin
   GTK_VSEPARATOR_CLASS:=PGtkVSeparatorClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_VSEPARATOR));
end;

function GTK_IS_VSEPARATOR(obj: pointer) : boolean;
begin
   GTK_IS_VSEPARATOR:=GTK_CHECK_TYPE(obj,GTK_TYPE_VSEPARATOR);
end;

function GTK_IS_VSEPARATOR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_VSEPARATOR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_VSEPARATOR);
end;

function GTK_VSEPARATOR_GET_CLASS(obj: pointer) : PGtkVSeparatorClass;
begin
   GTK_VSEPARATOR_GET_CLASS:=PGtkVSeparatorClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_VSEPARATOR));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

