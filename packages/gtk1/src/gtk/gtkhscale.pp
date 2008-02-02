{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkHScale = ^TGtkHScale;
     TGtkHScale = record
          scale : TGtkScale;
       end;

     PGtkHScaleClass = ^TGtkHScaleClass;
     TGtkHScaleClass = record
          parent_class : TGtkScaleClass;
       end;

Type
  GTK_HSCALE=PGtkHScale;
  GTK_HSCALE_CLASS=PGtkHScaleClass;

function  GTK_HSCALE_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_hscale_get_type';
function  GTK_IS_HSCALE(obj:pointer):boolean;
function  GTK_IS_HSCALE_CLASS(klass:pointer):boolean;

function  gtk_hscale_get_type:TGtkType;cdecl;external gtkdll name 'gtk_hscale_get_type';
function  gtk_hscale_new(adjustment:PGtkAdjustment):PGtkWidget;cdecl;external gtkdll name 'gtk_hscale_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_HSCALE(obj:pointer):boolean;
begin
  GTK_IS_HSCALE:=(obj<>nil) and GTK_IS_HSCALE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_HSCALE_CLASS(klass:pointer):boolean;
begin
  GTK_IS_HSCALE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_HSCALE_TYPE);
end;

{$endif read_implementation}


