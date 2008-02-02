// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkHSeparator = ^TGtkHSeparator;
   TGtkHSeparator = record
        separator : TGtkSeparator;
     end;

   PGtkHSeparatorClass = ^TGtkHSeparatorClass;
   TGtkHSeparatorClass = record
        parent_class : TGtkSeparatorClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_HSEPARATOR : GType;
function GTK_HSEPARATOR(obj: pointer) : PGtkHSeparator;
function GTK_HSEPARATOR_CLASS(klass: pointer) : PGtkHSeparatorClass;
function GTK_IS_HSEPARATOR(obj: pointer) : boolean;
function GTK_IS_HSEPARATOR_CLASS(klass: pointer) : boolean;
function GTK_HSEPARATOR_GET_CLASS(obj: pointer) : PGtkHSeparatorClass;


function gtk_hseparator_get_type:TGtkType; cdecl; external gtklib;
function gtk_hseparator_new:PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_HSEPARATOR : GType;
begin
   GTK_TYPE_HSEPARATOR:=gtk_hseparator_get_type;
end;

function GTK_HSEPARATOR(obj: pointer) : PGtkHSeparator;
begin
   GTK_HSEPARATOR:=PGtkHSeparator(GTK_CHECK_CAST(obj,GTK_TYPE_HSEPARATOR));
end;

function GTK_HSEPARATOR_CLASS(klass: pointer) : PGtkHSeparatorClass;
begin
   GTK_HSEPARATOR_CLASS:=PGtkHSeparatorClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_HSEPARATOR));
end;

function GTK_IS_HSEPARATOR(obj: pointer) : boolean;
begin
   GTK_IS_HSEPARATOR:=GTK_CHECK_TYPE(obj,GTK_TYPE_HSEPARATOR);
end;

function GTK_IS_HSEPARATOR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_HSEPARATOR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_HSEPARATOR);
end;

function GTK_HSEPARATOR_GET_CLASS(obj: pointer) : PGtkHSeparatorClass;
begin
   GTK_HSEPARATOR_GET_CLASS:=PGtkHSeparatorClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_HSEPARATOR));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

