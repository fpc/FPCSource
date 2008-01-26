// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkFixed = ^TGtkFixed;
   TGtkFixed = record
        container : TGtkContainer;
        children : PGList;
     end;

   PGtkFixedClass = ^TGtkFixedClass;
   TGtkFixedClass = record
        parent_class : TGtkContainerClass;
     end;

   PGtkFixedChild = ^TGtkFixedChild;
   TGtkFixedChild = record
        widget : PGtkWidget;
        x : gint;
        y : gint;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_FIXED : GType;
function GTK_FIXED(obj: pointer) : PGtkFixed;
function GTK_FIXED_CLASS(klass: pointer) : PGtkFixedClass;
function GTK_IS_FIXED(obj: pointer) : boolean;
function GTK_IS_FIXED_CLASS(klass: pointer) : boolean;
function GTK_FIXED_GET_CLASS(obj: pointer) : PGtkFixedClass;


function gtk_fixed_get_type:TGtkType; cdecl; external gtklib;
function gtk_fixed_new:PGtkWidget; cdecl; external gtklib;
procedure gtk_fixed_put(fixed:PGtkFixed; widget:PGtkWidget; x:gint; y:gint); cdecl; external gtklib;
procedure gtk_fixed_move(fixed:PGtkFixed; widget:PGtkWidget; x:gint; y:gint); cdecl; external gtklib;
procedure gtk_fixed_set_has_window(fixed:PGtkFixed; has_window:gboolean); cdecl; external gtklib;
function gtk_fixed_get_has_window(fixed:PGtkFixed):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_FIXED : GType;
begin
   GTK_TYPE_FIXED:=gtk_fixed_get_type;
end;

function GTK_FIXED(obj: pointer) : PGtkFixed;
begin
   GTK_FIXED:=PGtkFixed(GTK_CHECK_CAST(obj,GTK_TYPE_FIXED));
end;

function GTK_FIXED_CLASS(klass: pointer) : PGtkFixedClass;
begin
   GTK_FIXED_CLASS:=PGtkFixedClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_FIXED));
end;

function GTK_IS_FIXED(obj: pointer) : boolean;
begin
   GTK_IS_FIXED:=GTK_CHECK_TYPE(obj,GTK_TYPE_FIXED);
end;

function GTK_IS_FIXED_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_FIXED_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_FIXED);
end;

function GTK_FIXED_GET_CLASS(obj: pointer) : PGtkFixedClass;
begin
   GTK_FIXED_GET_CLASS:=PGtkFixedClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_FIXED));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

