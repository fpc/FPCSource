// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkHPaned = ^TGtkHPaned;
   TGtkHPaned = record
        paned : TGtkPaned;
     end;

   PGtkHPanedClass = ^TGtkHPanedClass;
   TGtkHPanedClass = record
        parent_class : TGtkPanedClass;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_HPANED : GType;
function GTK_HPANED(obj: pointer) : PGtkHPaned;
function GTK_HPANED_CLASS(klass: pointer) : PGtkHPanedClass;
function GTK_IS_HPANED(obj: pointer) : boolean;
function GTK_IS_HPANED_CLASS(klass: pointer) : boolean;
function GTK_HPANED_GET_CLASS(obj: pointer) : PGtkHPanedClass;



function gtk_hpaned_get_type:TGtkType; cdecl; external gtklib;
function gtk_hpaned_new:PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_HPANED : GType;
begin
   GTK_TYPE_HPANED:=gtk_hpaned_get_type;
end;

function GTK_HPANED(obj: pointer) : PGtkHPaned;
begin
   GTK_HPANED:=PGtkHPaned(GTK_CHECK_CAST(obj,GTK_TYPE_HPANED));
end;

function GTK_HPANED_CLASS(klass: pointer) : PGtkHPanedClass;
begin
   GTK_HPANED_CLASS:=PGtkHPanedClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_HPANED));
end;

function GTK_IS_HPANED(obj: pointer) : boolean;
begin
   GTK_IS_HPANED:=GTK_CHECK_TYPE(obj,GTK_TYPE_HPANED);
end;

function GTK_IS_HPANED_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_HPANED_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_HPANED);
end;

function GTK_HPANED_GET_CLASS(obj: pointer) : PGtkHPanedClass;
begin
   GTK_HPANED_GET_CLASS:=PGtkHPanedClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_HPANED));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

