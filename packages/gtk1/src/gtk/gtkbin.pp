{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkBin = ^TGtkBin;
       TGtkBin = record
            container : TGtkContainer;
            child : PGtkWidget;
         end;

       PGtkBinClass = ^TGtkBinClass;
       TGtkBinClass = record
            parent_class : TGtkContainerClass;
         end;

Type
  GTK_BIN=PGtkBin;
  GTK_BIN_CLASS=PGtkBinClass;

function  GTK_BIN_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_bin_get_type';
function  GTK_IS_BIN(obj:pointer):boolean;
function  GTK_IS_BIN_CLASS(klass:pointer):boolean;

function  gtk_bin_get_type:TGtkType;cdecl;external gtkdll name 'gtk_bin_get_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_BIN(obj:pointer):boolean;
begin
  GTK_IS_BIN:=(obj<>nil) and GTK_IS_BIN_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_BIN_CLASS(klass:pointer):boolean;
begin
  GTK_IS_BIN_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_BIN_TYPE);
end;

{$endif read_implementation}


