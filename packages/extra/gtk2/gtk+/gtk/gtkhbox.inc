// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkHBox = ^TGtkHBox;
   TGtkHBox = record
        box : TGtkBox;
     end;

   PGtkHBoxClass = ^TGtkHBoxClass;
   TGtkHBoxClass = record
        parent_class : TGtkBoxClass;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_HBOX : GType;
function GTK_HBOX(obj: pointer) : PGtkHBox;
function GTK_HBOX_CLASS(klass: pointer) : PGtkHBoxClass;
function GTK_IS_HBOX(obj: pointer) : boolean;
function GTK_IS_HBOX_CLASS(klass: pointer) : boolean;
function GTK_HBOX_GET_CLASS(obj: pointer) : PGtkHBoxClass;



function gtk_hbox_get_type:TGtkType; cdecl; external gtklib;
function gtk_hbox_new(homogeneous:gboolean; spacing:gint):PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_HBOX : GType;
begin
   GTK_TYPE_HBOX:=gtk_hbox_get_type;
end;

function GTK_HBOX(obj: pointer) : PGtkHBox;
begin
   GTK_HBOX:=PGtkHBox(GTK_CHECK_CAST(obj,GTK_TYPE_HBOX));
end;

function GTK_HBOX_CLASS(klass: pointer) : PGtkHBoxClass;
begin
   GTK_HBOX_CLASS:=PGtkHBoxClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_HBOX));
end;

function GTK_IS_HBOX(obj: pointer) : boolean;
begin
   GTK_IS_HBOX:=GTK_CHECK_TYPE(obj,GTK_TYPE_HBOX);
end;

function GTK_IS_HBOX_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_HBOX_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_HBOX);
end;

function GTK_HBOX_GET_CLASS(obj: pointer) : PGtkHBoxClass;
begin
   GTK_HBOX_GET_CLASS:=PGtkHBoxClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_HBOX));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

