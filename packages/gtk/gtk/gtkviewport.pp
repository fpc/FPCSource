{
   $Id$
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


{
  $Log$
  Revision 1.1  2000-07-13 06:34:07  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:13:23  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:39:44  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:20:43  peter
    * cdecl fixes

  Revision 1.5  1999/05/07 15:10:20  peter
    * more fixes

  Revision 1.4  1998/11/09 10:10:44  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:26  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

