{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkEventBox = ^TGtkEventBox;
     TGtkEventBox = record
          bin : TGtkBin;
       end;

     PGtkEventBoxClass = ^TGtkEventBoxClass;
     TGtkEventBoxClass = record
          parent_class : TGtkBinClass;
       end;

Type
  GTK_EVENT_BOX=PGtkEventBox;
  GTK_EVENT_BOX_CLASS=PGtkEventBoxClass;

function  GTK_EVENT_BOX_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_event_box_get_type';
function  GTK_IS_EVENT_BOX(obj:pointer):boolean;
function  GTK_IS_EVENT_BOX_CLASS(klass:pointer):boolean;

function  gtk_event_box_get_type:TGtkType;cdecl;external gtkdll name 'gtk_event_box_get_type';
function  gtk_event_box_new : PGtkWidget;cdecl;external gtkdll name 'gtk_event_box_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_EVENT_BOX(obj:pointer):boolean;
begin
  GTK_IS_EVENT_BOX:=(obj<>nil) and GTK_IS_EVENT_BOX_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_EVENT_BOX_CLASS(klass:pointer):boolean;
begin
  GTK_IS_EVENT_BOX_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_EVENT_BOX_TYPE);
end;

{$endif read_implementation}


