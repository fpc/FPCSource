// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkVPaned = ^TGtkVPaned;
   TGtkVPaned = record
        paned : TGtkPaned;
     end;

   PGtkVPanedClass = ^TGtkVPanedClass;
   TGtkVPanedClass = record
        parent_class : TGtkPanedClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_VPANED : GType;
function GTK_VPANED(obj: pointer) : PGtkVPaned;
function GTK_VPANED_CLASS(klass: pointer) : PGtkVPanedClass;
function GTK_IS_VPANED(obj: pointer) : boolean;
function GTK_IS_VPANED_CLASS(klass: pointer) : boolean;
function GTK_VPANED_GET_CLASS(obj: pointer) : PGtkVPanedClass;


function gtk_vpaned_get_type:TGtkType; cdecl; external gtklib;
function gtk_vpaned_new:PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_VPANED : GType;
begin
   GTK_TYPE_VPANED:=gtk_vpaned_get_type;
end;

function GTK_VPANED(obj: pointer) : PGtkVPaned;
begin
   GTK_VPANED:=PGtkVPaned(GTK_CHECK_CAST(obj,GTK_TYPE_VPANED));
end;

function GTK_VPANED_CLASS(klass: pointer) : PGtkVPanedClass;
begin
   GTK_VPANED_CLASS:=PGtkVPanedClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_VPANED));
end;

function GTK_IS_VPANED(obj: pointer) : boolean;
begin
   GTK_IS_VPANED:=GTK_CHECK_TYPE(obj,GTK_TYPE_VPANED);
end;

function GTK_IS_VPANED_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_VPANED_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_VPANED);
end;

function GTK_VPANED_GET_CLASS(obj: pointer) : PGtkVPanedClass;
begin
   GTK_VPANED_GET_CLASS:=PGtkVPanedClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_VPANED));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

