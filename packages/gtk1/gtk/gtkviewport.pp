{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkViewport = ^TGtkViewport;
     TGtkViewport = record
          bin : TGtkBin;
          shadow_type : TGtkShadowType;
          view_window : PGdkWindow;
          bin_window : PGdkWindow;
          hadjustment : PGtkAdjustment;
          vadjustment : PGtkAdjustment;
       end;

     PGtkViewportClass = ^TGtkViewportClass;
     TGtkViewportClass = record
          parent_class : TGtkBinClass;
          set_scroll_adjustments : procedure (viewport:PGtkViewport; hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment);cdecl;
       end;

Type
  GTK_VIEWPORT = PGtkViewport;
  GTK_VIEWPORT_CLASS = PGtkViewportClass;

function  GTK_VIEWPORT_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_viewport_get_type';
function  GTK_IS_VIEWPORT(obj:pointer):boolean;
function  GTK_IS_VIEWPORT_CLASS(klass:pointer):boolean;

function  gtk_viewport_get_type:TGtkType;cdecl;external gtkdll name 'gtk_viewport_get_type';
function  gtk_viewport_new(hadjustment:PGtkAdjustment; vadjustment:PGtkAdjustment):PGtkWidget;cdecl;external gtkdll name 'gtk_viewport_new';
function  gtk_viewport_get_hadjustment(viewport:PGtkViewport):PGtkAdjustment;cdecl;external gtkdll name 'gtk_viewport_get_hadjustment';
function  gtk_viewport_get_vadjustment(viewport:PGtkViewport):PGtkAdjustment;cdecl;external gtkdll name 'gtk_viewport_get_vadjustment';
procedure gtk_viewport_set_hadjustment(viewport:PGtkViewport; adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_viewport_set_hadjustment';
procedure gtk_viewport_set_vadjustment(viewport:PGtkViewport; adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_viewport_set_vadjustment';
procedure gtk_viewport_set_shadow_type(viewport:PGtkViewport; thetype:TGtkShadowType);cdecl;external gtkdll name 'gtk_viewport_set_shadow_type';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_VIEWPORT(obj:pointer):boolean;
begin
  GTK_IS_VIEWPORT:=(obj<>nil) and GTK_IS_VIEWPORT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_VIEWPORT_CLASS(klass:pointer):boolean;
begin
  GTK_IS_VIEWPORT_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_VIEWPORT_TYPE);
end;

{$endif read_implementation}


