
{$IFDEF read_forward_definitions}
{$ENDIF}

{$IFDEF read_interface_types}
type

   PGtkFileChooserProp = ^TGtkFileChooserProp;
   TGtkFileChooserProp = (GTK_FILE_CHOOSER_PROP_ACTION := $1000,
                          GTK_FILE_CHOOSER_PROP_FILE_SYSTEM_BACKEND,
                          GTK_FILE_CHOOSER_PROP_FILTER,
                          GTK_FILE_CHOOSER_PROP_LOCAL_ONLY,
                          GTK_FILE_CHOOSER_PROP_PREVIEW_WIDGET,
                          GTK_FILE_CHOOSER_PROP_PREVIEW_WIDGET_ACTIVE,
                          GTK_FILE_CHOOSER_PROP_USE_PREVIEW_LABEL,
                          GTK_FILE_CHOOSER_PROP_EXTRA_WIDGET,
                          GTK_FILE_CHOOSER_PROP_SELECT_MULTIPLE,
                          GTK_FILE_CHOOSER_PROP_SHOW_HIDDEN);
const
   GTK_FILE_CHOOSER_PROP_FIRST = GTK_FILE_CHOOSER_PROP_ACTION;
   GTK_FILE_CHOOSER_PROP_LAST = GTK_FILE_CHOOSER_PROP_SHOW_HIDDEN;

{$ENDIF}        {types}

{$IFDEF read_interface_rest}



procedure _gtk_file_chooser_install_properties(klass:PGObjectClass);cdecl;external gtklib name '_gtk_file_chooser_install_properties';
procedure _gtk_file_chooser_delegate_iface_init(iface:PGtkFileChooserIface);cdecl;external gtklib name '_gtk_file_chooser_delegate_iface_init';
procedure _gtk_file_chooser_set_delegate(receiver:PGtkFileChooser; delegate:PGtkFileChooser);cdecl;external gtklib name '_gtk_file_chooser_set_delegate';
{$endif}

{************************************************}

{$IFDEF read_implementation}

{$ENDIF}
