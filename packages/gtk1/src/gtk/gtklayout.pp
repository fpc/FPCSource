{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkLayout = ^TGtkLayout;
     TGtkLayout = record
          container : TGtkContainer;
          children : PGList;
          width : guint;
          height : guint;
          xoffset : guint;
          yoffset : guint;
          hadjustment : PGtkAdjustment;
          vadjustment : PGtkAdjustment;
          bin_window : PGdkWindow;
          visibility : TGdkVisibilityState;
          configure_serial : gulong;
          scroll_x : gint;
          scroll_y : gint;
          freeze_count : guint;
       end;

     PGtkLayoutClass = ^TGtkLayoutClass;
     TGtkLayoutClass = record
          parent_class : TGtkContainerClass;
          set_scroll_adjustments : procedure (layout:PGtkLayout; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment);cdecl;
       end;

type
  GTK_LAYOUT=PGtkLayout;
  GTK_LAYOUT_CLASS=PGtkLayoutClass;

function  GTK_LAYOUT_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_layout_get_type';
function  GTK_IS_LAYOUT(obj:pointer):boolean;
function  GTK_IS_LAYOUT_CLASS(klass:pointer):boolean;

function  gtk_layout_get_type:TGtkType;cdecl;external gtkdll name 'gtk_layout_get_type';
function  gtk_layout_new(hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment):PGtkWidget;cdecl;external gtkdll name 'gtk_layout_new';
procedure gtk_layout_put(layout:PGtkLayout; widget:PGtkWidget; x:gint; y:gint);cdecl;external gtkdll name 'gtk_layout_put';
procedure gtk_layout_move(layout:PGtkLayout; widget:PGtkWidget; x:gint; y:gint);cdecl;external gtkdll name 'gtk_layout_move';
procedure gtk_layout_set_size(layout:PGtkLayout; width:guint; height:guint);cdecl;external gtkdll name 'gtk_layout_set_size';
function  gtk_layout_get_hadjustment(layout:PGtkLayout):PGtkAdjustment;cdecl;external gtkdll name 'gtk_layout_get_hadjustment';
function  gtk_layout_get_vadjustment(layout:PGtkLayout):PGtkAdjustment;cdecl;external gtkdll name 'gtk_layout_get_vadjustment';
procedure gtk_layout_set_hadjustment(layout:PGtkLayout; adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_layout_set_hadjustment';
procedure gtk_layout_set_vadjustment(layout:PGtkLayout; adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_layout_set_vadjustment';
procedure gtk_layout_freeze(layout:PGtkLayout);cdecl;external gtkdll name 'gtk_layout_freeze';
procedure gtk_layout_thaw(layout:PGtkLayout);cdecl;external gtkdll name 'gtk_layout_thaw';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_LAYOUT(obj:pointer):boolean;
begin
  GTK_IS_LAYOUT:=(obj<>nil) and GTK_IS_LAYOUT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_LAYOUT_CLASS(klass:pointer):boolean;
begin
  GTK_IS_LAYOUT_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_LAYOUT_TYPE);
end;

{$endif read_implementation}


