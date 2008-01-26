{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkCheckButton = ^TGtkCheckButton;
       TGtkCheckButton = record
            toggle_button : TGtkToggleButton;
         end;

       PGtkCheckButtonClass = ^TGtkCheckButtonClass;
       TGtkCheckButtonClass = record
            parent_class : TGtkToggleButtonClass;
            indicator_size : guint16;
            indicator_spacing : guint16;
            draw_indicator : procedure (check_button:PGtkCheckButton; area:PGdkRectangle); cdecl;
         end;

Type
  GTK_CHECK_BUTTON=PGtkCheckButton;
  GTK_CHECK_BUTTON_CLASS=PGtkCheckButtonClass;

function  GTK_CHECK_BUTTON_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_check_button_get_type';
function  GTK_IS_CHECK_BUTTON(obj:pointer):boolean;
function  GTK_IS_CHECK_BUTTON_CLASS(klass:pointer):boolean;

function  gtk_check_button_get_type:TGtkType;cdecl;external gtkdll name 'gtk_check_button_get_type';
function  gtk_check_button_new : PGtkWidget;cdecl;external gtkdll name 'gtk_check_button_new';
function  gtk_check_button_new_with_label (thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_check_button_new_with_label';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_CHECK_BUTTON(obj:pointer):boolean;
begin
  GTK_IS_CHECK_BUTTON:=(obj<>nil) and GTK_IS_CHECK_BUTTON_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_CHECK_BUTTON_CLASS(klass:pointer):boolean;
begin
  GTK_IS_CHECK_BUTTON_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_CHECK_BUTTON_TYPE);
end;

{$endif read_implementation}


