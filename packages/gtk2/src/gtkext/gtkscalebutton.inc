
function GTK_TYPE_SCALE_BUTTON: GType; inline;
begin
  GTK_TYPE_SCALE_BUTTON:=gtk_scale_button_get_type();
end;

function GTK_SCALE_BUTTON(obj : pointer) : PGTypeInstance; inline;
begin
   GTK_SCALE_BUTTON:=G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_SCALE_BUTTON);
end;

function GTK_SCALE_BUTTON_CLASS(klass : pointer) : pointer; inline;
begin
   GTK_SCALE_BUTTON_CLASS:=G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_SCALE_BUTTON);
end;

function GTK_IS_SCALE_BUTTON(obj : pointer) : boolean; inline;
begin
   GTK_IS_SCALE_BUTTON:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_SCALE_BUTTON);
end;

function GTK_IS_SCALE_BUTTON_CLASS(klass : pointer) : boolean; inline;
begin
   GTK_IS_SCALE_BUTTON_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_SCALE_BUTTON);
end;

function GTK_SCALE_BUTTON_GET_CLASS(obj : pointer) : PGTypeClass; inline;
begin
   GTK_SCALE_BUTTON_GET_CLASS:=G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_SCALE_BUTTON);
end;

function Available_GtkScaleButton: Boolean;
begin
  Result := Assigned(gtk_scale_button_new);
end;

procedure Freegtkscalebutton;
begin
  gtk_scale_button_get_type:=nil;
  gtk_scale_button_new:=nil;
  gtk_scale_button_set_icons:=nil;
  gtk_scale_button_get_value:=nil;
  gtk_scale_button_set_value:=nil;
  gtk_scale_button_get_adjustment:=nil;
  gtk_scale_button_set_adjustment:=nil;
end;

procedure Loadgtkscalebutton;
begin
  Freegtkscalebutton;

  pointer(gtk_scale_button_get_type):=GetProcAddress(gtkhandle,'gtk_scale_button_get_type');
  pointer(gtk_scale_button_new):=GetProcAddress(gtkhandle,'gtk_scale_button_new');
  pointer(gtk_scale_button_set_icons):=GetProcAddress(gtkhandle,'gtk_scale_button_set_icons');
  pointer(gtk_scale_button_get_value):=GetProcAddress(gtkhandle,'gtk_scale_button_get_value');
  pointer(gtk_scale_button_set_value):=GetProcAddress(gtkhandle,'gtk_scale_button_set_value');
  pointer(gtk_scale_button_get_adjustment):=GetProcAddress(gtkhandle,'gtk_scale_button_get_adjustment');
  pointer(gtk_scale_button_set_adjustment):=GetProcAddress(gtkhandle,'gtk_scale_button_set_adjustment');
end;

