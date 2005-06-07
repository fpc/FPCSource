{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkData = ^TGtkData;
       TGtkData = record
            theobject : TGtkObject;
         end;

       DisconnectProc  = procedure (data : PGtkdata);cdecl;

       PGtkDataClass = ^TGtkDataClass;
       TGtkDataClass = record
            parent_class : TGtkObjectClass;
            disconnect : disconnectproc;
         end;

Type
  GTK_DATA=PGtkData;
  GTK_DATA_CLASS=PGtkDataClass;

function  GTK_DATA_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_data_get_type';
function  GTK_IS_DATA(obj:pointer):boolean;
function  GTK_IS_DATA_CLASS(klass:pointer):boolean;

function  gtk_data_get_type:TGtkType;cdecl;external gtkdll name 'gtk_data_get_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_DATA(obj:pointer):boolean;
begin
  GTK_IS_DATA:=(obj<>nil) and GTK_IS_DATA_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_DATA_CLASS(klass:pointer):boolean;
begin
  GTK_IS_DATA_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_DATA_TYPE);
end;

{$endif read_implementation}


