
type
  PGtkVolumeButtonClass = pointer;
  PGtkVolumeButton = pointer;

var
  gtk_volume_button_get_type: function:GType;cdecl;
  gtk_volume_button_new : function:PGtkWidget;cdecl;

  function GTK_TYPE_VOLUME_BUTTON: GType; inline;
  function GTK_VOLUME_BUTTON(obj : pointer) : PGTypeInstance; inline;
  function GTK_VOLUME_BUTTON_CLASS(klass : pointer) : pointer; inline;
  function GTK_IS_VOLUME_BUTTON(obj : pointer) : boolean; inline;
  function GTK_IS_VOLUME_BUTTON_CLASS(klass : pointer) : boolean; inline;
  function GTK_VOLUME_BUTTON_GET_CLASS(obj : pointer) : PGTypeClass; inline;

  function Available_GtkVolumeButton: Boolean;

