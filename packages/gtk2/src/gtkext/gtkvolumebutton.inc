procedure Freegtkvolumebutton;
begin
  gtk_volume_button_new:=nil;
  gtk_volume_button_get_type:=nil;
end;


procedure Loadgtkvolumebutton;
begin
  Freegtkvolumebutton;

  pointer(gtk_volume_button_get_type):=GetProcAddress(gtkhandle,'gtk_volume_button_get_type');
  pointer(gtk_volume_button_new):=GetProcAddress(gtkhandle,'gtk_volume_button_new');
end;

function GTK_TYPE_VOLUME_BUTTON: GType; inline;
begin
  GTK_TYPE_VOLUME_BUTTON := gtk_volume_button_get_type();
end;

function GTK_VOLUME_BUTTON(obj : pointer) : PGTypeInstance;
begin
   GTK_VOLUME_BUTTON:=G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_VOLUME_BUTTON);
end;

function GTK_VOLUME_BUTTON_CLASS(klass : pointer) : pointer;
begin
   GTK_VOLUME_BUTTON_CLASS:=G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_VOLUME_BUTTON);
end;

function GTK_IS_VOLUME_BUTTON(obj : pointer) : boolean; inline;
begin
   GTK_IS_VOLUME_BUTTON:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_VOLUME_BUTTON);
end;

function GTK_IS_VOLUME_BUTTON_CLASS(klass : pointer) : boolean; inline;
begin
   GTK_IS_VOLUME_BUTTON_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_VOLUME_BUTTON);
end;

function GTK_VOLUME_BUTTON_GET_CLASS(obj : pointer) : PGTypeClass; inline;
begin
   GTK_VOLUME_BUTTON_GET_CLASS:=G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_VOLUME_BUTTON);
end;

function Available_GtkVolumeButton: Boolean;
begin
  Result := Assigned(gtk_volume_button_new);
end;

