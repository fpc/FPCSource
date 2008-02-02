// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkBin = ^TGtkBin;
   TGtkBin = record
        container : TGtkContainer;
        child : PGtkWidget;
     end;

   PGtkBinClass = ^TGtkBinClass;
   TGtkBinClass = record
        parent_class : TGtkContainerClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_BIN : GType;
function GTK_BIN(obj: pointer) : PGtkBin;
function GTK_BIN_CLASS(klass: pointer) : PGtkBinClass;
function GTK_IS_BIN(obj: pointer) : boolean;
function GTK_IS_BIN_CLASS(klass: pointer) : boolean;
function GTK_BIN_GET_CLASS(obj: pointer) : PGtkBinClass;


function gtk_bin_get_type:TGtkType; cdecl; external gtklib;
function gtk_bin_get_child(bin:PGtkBin):PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_BIN : GType;
begin
   GTK_TYPE_BIN:=gtk_bin_get_type;
end;

function GTK_BIN(obj: pointer) : PGtkBin;
begin
   GTK_BIN:=PGtkBin(GTK_CHECK_CAST(obj,GTK_TYPE_BIN));
end;

function GTK_BIN_CLASS(klass: pointer) : PGtkBinClass;
begin
   GTK_BIN_CLASS:=PGtkBinClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_BIN));
end;

function GTK_IS_BIN(obj: pointer) : boolean;
begin
   GTK_IS_BIN:=GTK_CHECK_TYPE(obj,GTK_TYPE_BIN);
end;

function GTK_IS_BIN_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_BIN_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_BIN);
end;

function GTK_BIN_GET_CLASS(obj: pointer) : PGtkBinClass;
begin
   GTK_BIN_GET_CLASS:=PGtkBinClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_BIN));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

