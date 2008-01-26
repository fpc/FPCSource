{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkHButtonBox = ^TGtkHButtonBox;
     TGtkHButtonBox = record
          button_box : TGtkButtonBox;
       end;

     PGtkHButtonBoxClass = ^TGtkHButtonBoxClass;
     TGtkHButtonBoxClass = record
          parent_class : TGtkButtonBoxClass;
       end;

Type
  GTK_HBUTTON_BOX=PGtkHButtonBox;
  GTK_HBUTTON_BOX_CLASS=PGtkHButtonBoxClass;

function  GTK_HBUTTON_BOX_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_hbutton_box_get_type';
function  GTK_IS_HBUTTON_BOX(obj:pointer):boolean;
function  GTK_IS_HBUTTON_BOX_CLASS(klass:pointer):boolean;

function  gtk_hbutton_box_get_type:TGtkType;cdecl;external gtkdll name 'gtk_hbutton_box_get_type';
function  gtk_hbutton_box_new :PGtkWidget;cdecl;external gtkdll name 'gtk_hbutton_box_new';
function  gtk_hbutton_box_get_spacing_default:gint;cdecl;external gtkdll name 'gtk_hbutton_box_get_spacing_default';
function  gtk_hbutton_box_get_layout_default:TGtkButtonBoxStyle;cdecl;external gtkdll name 'gtk_hbutton_box_get_layout_default';
procedure gtk_hbutton_box_set_spacing_default(spacing:gint);cdecl;external gtkdll name 'gtk_hbutton_box_set_spacing_default';
procedure gtk_hbutton_box_set_layout_default(layout:TGtkButtonBoxStyle);cdecl;external gtkdll name 'gtk_hbutton_box_set_layout_default';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_HBUTTON_BOX(obj:pointer):boolean;
begin
  GTK_IS_HBUTTON_BOX:=(obj<>nil) and GTK_IS_HBUTTON_BOX_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_HBUTTON_BOX_CLASS(klass:pointer):boolean;
begin
  GTK_IS_HBUTTON_BOX_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_HBUTTON_BOX_TYPE);
end;

{$endif read_implementation}


