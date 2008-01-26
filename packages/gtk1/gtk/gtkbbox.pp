{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    const
       GTK_BUTTONBOX_DEFAULT = -1;

    type
       PGtkButtonBox = ^TGtkButtonBox;
       TGtkButtonBox = record
            box : TGtkBox;
            spacing : gint;
            child_min_width : gint;
            child_min_height : gint;
            child_ipad_x : gint;
            child_ipad_y : gint;
            layout_style : TGtkButtonBoxStyle;
         end;

       PGtkButtonBoxClass = ^TGtkButtonBoxClass;
       TGtkButtonBoxClass = record
            parent_class : TGtkBoxClass;
         end;

Type
  GTK_BUTTON_BOX=PGtkButtonBox;
  GTK_BUTTON_BOX_CLASS=PGtkButtonBoxClass;

function  GTK_BUTTON_BOX_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_button_box_get_type';
function  GTK_IS_BUTTON_BOX(obj:pointer):boolean;
function  GTK_IS_BUTTON_BOX_CLASS(klass:pointer):boolean;

function  gtk_button_box_get_type:TGtktype;cdecl;external gtkdll name 'gtk_button_box_get_type';
procedure gtk_button_box_get_child_size_default(min_width:Pgint; min_height:Pgint);cdecl;external gtkdll name 'gtk_button_box_get_child_size_default';
procedure gtk_button_box_get_child_ipadding_default(ipad_x:Pgint; ipad_y:Pgint);cdecl;external gtkdll name 'gtk_button_box_get_child_ipadding_default';
procedure gtk_button_box_set_child_size_default(min_width:gint; min_height:gint);cdecl;external gtkdll name 'gtk_button_box_set_child_size_default';
procedure gtk_button_box_set_child_ipadding_default(ipad_x:gint; ipad_y:gint);cdecl;external gtkdll name 'gtk_button_box_set_child_ipadding_default';
function  gtk_button_box_get_spacing(widget:PGtkButtonBox):gint;cdecl;external gtkdll name 'gtk_button_box_get_spacing';
function  gtk_button_box_get_layout(widget:PGtkButtonBox):TGtkButtonBoxStyle;cdecl;external gtkdll name 'gtk_button_box_get_layout';
procedure gtk_button_box_get_child_size(widget:PGtkButtonBox; min_width:Pgint; min_height:Pgint);cdecl;external gtkdll name 'gtk_button_box_get_child_size';
procedure gtk_button_box_get_child_ipadding(widget:PGtkButtonBox; ipad_x:Pgint; ipad_y:Pgint);cdecl;external gtkdll name 'gtk_button_box_get_child_ipadding';
procedure gtk_button_box_set_spacing(widget:PGtkButtonBox; spacing:gint);cdecl;external gtkdll name 'gtk_button_box_set_spacing';
procedure gtk_button_box_set_layout(widget:PGtkButtonBox; layout_style:TGtkButtonBoxStyle);cdecl;external gtkdll name 'gtk_button_box_set_layout';
procedure gtk_button_box_set_child_size(widget:PGtkButtonBox; min_width:gint; min_height:gint);cdecl;external gtkdll name 'gtk_button_box_set_child_size';
procedure gtk_button_box_set_child_ipadding(widget:PGtkButtonBox; ipad_x:gint; ipad_y:gint);cdecl;external gtkdll name 'gtk_button_box_set_child_ipadding';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_BUTTON_BOX(obj:pointer):boolean;
begin
  GTK_IS_BUTTON_BOX:=(obj<>nil) and GTK_IS_BUTTON_BOX_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_BUTTON_BOX_CLASS(klass:pointer):boolean;
begin
  GTK_IS_BUTTON_BOX_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_BUTTON_BOX_TYPE);
end;

{$endif read_implementation}

