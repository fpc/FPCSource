{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     TGtkToolbarChildType = longint;
  const
     GTK_TOOLBAR_CHILD_SPACE = 0;
     GTK_TOOLBAR_CHILD_BUTTON = 1;
     GTK_TOOLBAR_CHILD_TOGGLEBUTTON = 2;
     GTK_TOOLBAR_CHILD_RADIOBUTTON = 3;
     GTK_TOOLBAR_CHILD_WIDGET = 4;

  type
     TGtkToolbarSpaceStyle = longint;
  const
     GTK_TOOLBAR_SPACE_EMPTY = 0;
     GTK_TOOLBAR_SPACE_LINE = 1;

  type
     PGtkToolbarChild = ^TGtkToolbarChild;
     TGtkToolbarChild = record
          thetype : TGtkToolbarChildType;
          widget : PGtkWidget;
          icon : PGtkWidget;
          thelabel : PGtkWidget;
       end;

     PGtkToolbar = ^TGtkToolbar;
     TGtkToolbar = record
          container : TGtkContainer;
          num_children : gint;
          children : PGList;
          orientation : TGtkOrientation;
          style : TGtkToolbarStyle;
          space_size : gint;
          space_style : TGtkToolbarSpaceStyle;
          tooltips : PGtkTooltips;
          button_maxw : gint;
          button_maxh : gint;
          relief : TGtkReliefStyle;
       end;

     PGtkToolbarClass = ^TGtkToolbarClass;
     TGtkToolbarClass = record
          parent_class : TGtkContainerClass;
          orientation_changed : procedure (toolbar:PGtkToolbar; orientation:TGtkOrientation); cdecl;
          style_changed : procedure (toolbar:PGtkToolbar; style:TGtkToolbarStyle); cdecl;
       end;

Type
  GTK_TOOLBAR=PGtkToolbar;
  GTK_TOOLBAR_CLASS=PGtkToolbarClass;

function  gtk_toolbar_get_type:guint;cdecl;external gtkdll name 'gtk_toolbar_get_type';
function  gtk_toolbar_new(orientation:TGtkOrientation; style:TGtkToolbarStyle):PGtkWidget;cdecl;external gtkdll name 'gtk_toolbar_new';
function  gtk_toolbar_append_item(toolbar:PGtkToolbar; thetext:pchar; tooltip_text:pchar; tooltip_private_text:pchar; icon:PGtkWidget; callback:TGtkSignalFunc; user_data:gpointer):PGtkWidget;cdecl;external gtkdll name 'gtk_toolbar_append_item';
function  gtk_toolbar_prepend_item(toolbar:PGtkToolbar; thetext:pchar; tooltip_text:pchar; tooltip_private_text:pchar; icon:PGtkWidget; callback:TGtkSignalFunc; user_data:gpointer):PGtkWidget;cdecl;external gtkdll name 'gtk_toolbar_prepend_item';
function  gtk_toolbar_insert_item(toolbar:PGtkToolbar; thetext:pchar; tooltip_text:pchar; tooltip_private_text:pchar; icon:PGtkWidget; callback:TGtkSignalFunc; user_data:gpointer; position:gint):PGtkWidget;cdecl;external gtkdll name 'gtk_toolbar_insert_item';
procedure gtk_toolbar_append_space(toolbar:PGtkToolbar);cdecl;external gtkdll name 'gtk_toolbar_append_space';
procedure gtk_toolbar_prepend_space(toolbar:PGtkToolbar);cdecl;external gtkdll name 'gtk_toolbar_prepend_space';
procedure gtk_toolbar_insert_space(toolbar:PGtkToolbar; position:gint);cdecl;external gtkdll name 'gtk_toolbar_insert_space';
function  gtk_toolbar_append_element(toolbar:PGtkToolbar; thetype:TGtkToolbarChildType; widget:PGtkWidget; thetext:pchar; tooltip_text:pchar;tooltip_private_text:pchar; icon:PGtkWidget; callback:TGtkSignalFunc; user_data:gpointer):PGtkWidget;cdecl;external gtkdll name 'gtk_toolbar_append_element';
function  gtk_toolbar_prepend_element(toolbar:PGtkToolbar; thetype:TGtkToolbarChildType; widget:PGtkWidget; thetext:pchar; tooltip_text:pchar;tooltip_private_text:pchar; icon:PGtkWidget; callback:TGtkSignalFunc; user_data:gpointer):PGtkWidget; cdecl;external gtkdll name 'gtk_toolbar_prepend_element';
function  gtk_toolbar_insert_element(toolbar:PGtkToolbar; thetype:TGtkToolbarChildType; widget:PGtkWidget; thetext:pchar; tooltip_text:pchar;tooltip_private_text:pchar; icon:PGtkWidget; callback:TGtkSignalFunc; user_data:gpointer; position:gint):PGtkWidget;cdecl;external gtkdll name 'gtk_toolbar_insert_element';
procedure gtk_toolbar_append_widget(toolbar:PGtkToolbar; widget:PGtkWidget; tooltip_text:pchar; tooltip_private_text:pchar);cdecl;external gtkdll name 'gtk_toolbar_append_widget';
procedure gtk_toolbar_prepend_widget(toolbar:PGtkToolbar; widget:PGtkWidget; tooltip_text:pchar; tooltip_private_text:pchar);cdecl;external gtkdll name 'gtk_toolbar_prepend_widget';
procedure gtk_toolbar_insert_widget(toolbar:PGtkToolbar; widget:PGtkWidget; tooltip_text:pchar; tooltip_private_text:pchar; position:gint);cdecl;external gtkdll name 'gtk_toolbar_insert_widget';
procedure gtk_toolbar_set_orientation(toolbar:PGtkToolbar; orientation:TGtkOrientation);cdecl;external gtkdll name 'gtk_toolbar_set_orientation';
procedure gtk_toolbar_set_style(toolbar:PGtkToolbar; style:TGtkToolbarStyle);cdecl;external gtkdll name 'gtk_toolbar_set_style';
procedure gtk_toolbar_set_space_size(toolbar:PGtkToolbar; space_size:gint);cdecl;external gtkdll name 'gtk_toolbar_set_space_size';
procedure gtk_toolbar_set_space_style(toolbar:PGtkToolbar; space_style:TGtkToolbarSpaceStyle);cdecl;external gtkdll name 'gtk_toolbar_set_space_style';
procedure gtk_toolbar_set_tooltips(toolbar:PGtkToolbar; enable:gint);cdecl;external gtkdll name 'gtk_toolbar_set_tooltips';
procedure gtk_toolbar_set_button_relief(toolbar:PGtkToolbar; relief:TGtkReliefStyle);cdecl;external gtkdll name 'gtk_toolbar_set_button_relief';
function  gtk_toolbar_get_button_relief(toolbar:PGtkToolbar):TGtkReliefStyle;cdecl;external gtkdll name 'gtk_toolbar_get_button_relief';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


