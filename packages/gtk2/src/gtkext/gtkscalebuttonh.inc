
type
  PGtkScaleButton = pointer;
  PGtkScaleButtonClass = pointer;

var
  gtk_scale_button_get_type : function: GType;cdecl;
  gtk_scale_button_new : function(size:TGtkIconSize; min:gdouble; max:gdouble; step:gdouble; icons:PPgchar):PGtkWidget;cdecl;
  gtk_scale_button_set_icons : procedure(button:PGtkScaleButton; icons:PPgchar);cdecl;
  gtk_scale_button_get_value : function(button:PGtkScaleButton):gdouble;cdecl;
  gtk_scale_button_set_value : procedure(button:PGtkScaleButton; value:gdouble);cdecl;
  gtk_scale_button_get_adjustment : function(button:PGtkScaleButton):PGtkAdjustment;cdecl;
  gtk_scale_button_set_adjustment : procedure(button:PGtkScaleButton; adjustment:PGtkAdjustment);cdecl;

  function GTK_TYPE_SCALE_BUTTON: GType; inline;
  function GTK_SCALE_BUTTON(obj : pointer) : PGTypeInstance; inline;
  function GTK_SCALE_BUTTON_CLASS(klass : pointer) : pointer; inline;
  function GTK_IS_SCALE_BUTTON(obj : pointer) : boolean; inline;
  function GTK_IS_SCALE_BUTTON_CLASS(klass : pointer) : boolean; inline;
  function GTK_SCALE_BUTTON_GET_CLASS(obj : pointer) : PGTypeClass; inline;

  function Available_GtkScaleButton: Boolean;

