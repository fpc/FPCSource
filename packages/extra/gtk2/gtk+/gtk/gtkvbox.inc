// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkVBox = ^TGtkVBox;
   TGtkVBox = record
        box : TGtkBox;
     end;

   PGtkVBoxClass = ^TGtkVBoxClass;
   TGtkVBoxClass = record
        parent_class : TGtkBoxClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_VBOX : GType;
function GTK_VBOX(obj: pointer) : PGtkVBox;
function GTK_VBOX_CLASS(klass: pointer) : PGtkVBoxClass;
function GTK_IS_VBOX(obj: pointer) : boolean;
function GTK_IS_VBOX_CLASS(klass: pointer) : boolean;
function GTK_VBOX_GET_CLASS(obj: pointer) : PGtkVBoxClass;


function gtk_vbox_get_type:TGtkType; cdecl; external gtklib;
function gtk_vbox_new(homogeneous:gboolean; spacing:gint):PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_VBOX : GType;
begin
   GTK_TYPE_VBOX:=gtk_vbox_get_type;
end;

function GTK_VBOX(obj: pointer) : PGtkVBox;
begin
   GTK_VBOX:=PGtkVBox(GTK_CHECK_CAST(obj,GTK_TYPE_VBOX));
end;

function GTK_VBOX_CLASS(klass: pointer) : PGtkVBoxClass;
begin
   GTK_VBOX_CLASS:=PGtkVBoxClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_VBOX));
end;

function GTK_IS_VBOX(obj: pointer) : boolean;
begin
   GTK_IS_VBOX:=GTK_CHECK_TYPE(obj,GTK_TYPE_VBOX);
end;

function GTK_IS_VBOX_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_VBOX_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_VBOX);
end;

function GTK_VBOX_GET_CLASS(obj: pointer) : PGtkVBoxClass;
begin
   GTK_VBOX_GET_CLASS:=PGtkVBoxClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_VBOX));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

