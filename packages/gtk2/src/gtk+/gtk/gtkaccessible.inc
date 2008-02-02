// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{
     This anObject is a thin wrapper, in the GTK+ namespace, for AtkObject
    }
{
     The GtkWidget whose properties and features are exported via this
     accessible instance.
    }
   PGtkAccessible = ^TGtkAccessible;
   TGtkAccessible = record
        parent : TAtkObject;
        widget : PGtkWidget;
     end;

{ Padding for future expansion  }
   PGtkAccessibleClass = ^TGtkAccessibleClass;
   TGtkAccessibleClass = record
        parent_class : TAtkObjectClass;
        connect_widget_destroyed : procedure (accessible:PGtkAccessible); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_ACCESSIBLE : GType;
function GTK_ACCESSIBLE(obj: pointer) : PGtkAccessible;
function GTK_ACCESSIBLE_CLASS(klass: pointer) : PGtkAccessibleClass;
function GTK_IS_ACCESSIBLE(obj: pointer) : boolean;
function GTK_IS_ACCESSIBLE_CLASS(klass: pointer) : boolean;
function GTK_ACCESSIBLE_GET_CLASS(obj: pointer) : PGtkAccessibleClass;



function gtk_accessible_get_type:TGtkType; cdecl; external gtklib;


procedure gtk_accessible_connect_widget_destroyed(accessible:PGtkAccessible); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ACCESSIBLE : GType;
begin
   GTK_TYPE_ACCESSIBLE:=gtk_accessible_get_type;
end;

function GTK_ACCESSIBLE(obj: pointer) : PGtkAccessible;
begin
   GTK_ACCESSIBLE:=PGtkAccessible(GTK_CHECK_CAST(obj,GTK_TYPE_ACCESSIBLE));
end;

function GTK_ACCESSIBLE_CLASS(klass: pointer) : PGtkAccessibleClass;
begin
   GTK_ACCESSIBLE_CLASS:=PGtkAccessibleClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_ACCESSIBLE));
end;

function GTK_IS_ACCESSIBLE(obj: pointer) : boolean;
begin
   GTK_IS_ACCESSIBLE:=GTK_CHECK_TYPE(obj,GTK_TYPE_ACCESSIBLE);
end;

function GTK_IS_ACCESSIBLE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ACCESSIBLE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_ACCESSIBLE);
end;

function GTK_ACCESSIBLE_GET_CLASS(obj: pointer) : PGtkAccessibleClass;
begin
   GTK_ACCESSIBLE_GET_CLASS:=PGtkAccessibleClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_ACCESSIBLE));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

