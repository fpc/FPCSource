{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkRadioButton = ^TGtkRadioButton;
     TGtkRadioButton = record
          check_button : TGtkCheckButton;
          group : PGSList;
       end;

     PGtkRadioButtonClass = ^TGtkRadioButtonClass;
     TGtkRadioButtonClass = record
          parent_class : TGtkCheckButtonClass;
       end;

Type
  GTK_RADIO_BUTTON=PGtkRadioButton;
  GTK_RADIO_BUTTON_CLASS=PGtkRadioButtonClass;

function  GTK_RADIO_BUTTON_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_radio_button_get_type';
function  GTK_IS_RADIO_BUTTON(obj:pointer):boolean;
function  GTK_IS_RADIO_BUTTON_CLASS(klass:pointer):boolean;

function  gtk_radio_button_get_type:TGtkType;cdecl;external gtkdll name 'gtk_radio_button_get_type';
function  gtk_radio_button_new(group:PGSList):PGtkWidget;cdecl;external gtkdll name 'gtk_radio_button_new';
function  gtk_radio_button_new_from_widget(group:PGtkRadioButton):PGtkWidget;cdecl;external gtkdll name 'gtk_radio_button_new_from_widget';
function  gtk_radio_button_new_with_label(group:PGSList; thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_radio_button_new_with_label';
function  gtk_radio_button_new_with_label_from_widget(group:PGtkRadioButton; thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_radio_button_new_with_label_from_widget';
function  gtk_radio_button_group(radio_button:PGtkRadioButton):PGSList;cdecl;external gtkdll name 'gtk_radio_button_group';
procedure gtk_radio_button_set_group(radio_button:PGtkRadioButton; group:PGSList);cdecl;external gtkdll name 'gtk_radio_button_set_group';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_RADIO_BUTTON(obj:pointer):boolean;
begin
  GTK_IS_RADIO_BUTTON:=(obj<>nil) and GTK_IS_RADIO_BUTTON_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_RADIO_BUTTON_CLASS(klass:pointer):boolean;
begin
  GTK_IS_RADIO_BUTTON_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_RADIO_BUTTON_TYPE);
end;

{$endif read_implementation}


