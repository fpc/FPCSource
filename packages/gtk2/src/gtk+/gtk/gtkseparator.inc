// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkSeparator = ^TGtkSeparator;
   TGtkSeparator = record
        widget : TGtkWidget;
     end;

   PGtkSeparatorClass = ^TGtkSeparatorClass;
   TGtkSeparatorClass = record
        parent_class : TGtkWidgetClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_SEPARATOR : GType;
function GTK_SEPARATOR(obj: pointer) : PGtkSeparator;
function GTK_SEPARATOR_CLASS(klass: pointer) : PGtkSeparatorClass;
function GTK_IS_SEPARATOR(obj: pointer) : boolean;
function GTK_IS_SEPARATOR_CLASS(klass: pointer) : boolean;
function GTK_SEPARATOR_GET_CLASS(obj: pointer) : PGtkSeparatorClass;


function gtk_separator_get_type:TGtkType; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_SEPARATOR : GType;
begin
   GTK_TYPE_SEPARATOR:=gtk_separator_get_type;
end;

function GTK_SEPARATOR(obj: pointer) : PGtkSeparator;
begin
   GTK_SEPARATOR:=PGtkSeparator(GTK_CHECK_CAST(obj,GTK_TYPE_SEPARATOR));
end;

function GTK_SEPARATOR_CLASS(klass: pointer) : PGtkSeparatorClass;
begin
   GTK_SEPARATOR_CLASS:=PGtkSeparatorClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_SEPARATOR));
end;

function GTK_IS_SEPARATOR(obj: pointer) : boolean;
begin
   GTK_IS_SEPARATOR:=GTK_CHECK_TYPE(obj,GTK_TYPE_SEPARATOR);
end;

function GTK_IS_SEPARATOR_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_SEPARATOR_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_SEPARATOR);
end;

function GTK_SEPARATOR_GET_CLASS(obj: pointer) : PGtkSeparatorClass;
begin
   GTK_SEPARATOR_GET_CLASS:=PGtkSeparatorClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_SEPARATOR));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

