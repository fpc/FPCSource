{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkHandleBox = ^TGtkHandleBox;
     TGtkHandleBox = record
          bin : TGtkBin;
          bin_window : PGdkWindow;
          float_window : PGdkWindow;
          shadow_type : TGtkShadowType;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          deskoff_x : gint;
          deskoff_y : gint;
          attach_allocation : TGtkAllocation;
          float_allocation : TGtkAllocation;
       end;

  const
     bm_TGtkHandleBox_handle_position = $3;
     bp_TGtkHandleBox_handle_position = 0;
     bm_TGtkHandleBox_float_window_mapped = $4;
     bp_TGtkHandleBox_float_window_mapped = 2;
     bm_TGtkHandleBox_child_detached = $8;
     bp_TGtkHandleBox_child_detached = 3;
     bm_TGtkHandleBox_in_drag = $10;
     bp_TGtkHandleBox_in_drag = 4;
     bm_TGtkHandleBox_shrink_on_detach = $20;
     bp_TGtkHandleBox_shrink_on_detach = 5;
     bm_TGtkHandleBox_snap_edge = $1C0;
     bp_TGtkHandleBox_snap_edge = 6;
function  handle_position(var a : TGtkHandleBox) : guint;
procedure set_handle_position(var a : TGtkHandleBox; __handle_position : guint);
function  float_window_mapped(var a : TGtkHandleBox) : guint;
procedure set_float_window_mapped(var a : TGtkHandleBox; __float_window_mapped : guint);
function  child_detached(var a : TGtkHandleBox) : guint;
procedure set_child_detached(var a : TGtkHandleBox; __child_detached : guint);
function  in_drag(var a : TGtkHandleBox) : guint;
procedure set_in_drag(var a : TGtkHandleBox; __in_drag : guint);
function  shrink_on_detach(var a : TGtkHandleBox) : guint;
procedure set_shrink_on_detach(var a : TGtkHandleBox; __shrink_on_detach : guint);
function  snap_edge(var a : TGtkHandleBox) : gint;
procedure set_snap_edge(var a : TGtkHandleBox; __snap_edge : gint);

  type
     PGtkHandleBoxClass = ^TGtkHandleBoxClass;
     TGtkHandleBoxClass = record
          parent_class : TGtkBinClass;
          child_attached : procedure (handle_box:PGtkHandleBox; child:PGtkWidget); cdecl;
          child_detached : procedure (handle_box:PGtkHandleBox; child:PGtkWidget); cdecl;
       end;

Type
  GTK_HANDLE_BOX=PGtkHandleBox;
  GTK_HANDLE_BOX_CLASS=PGtkHandleBoxClass;

function  GTK_HANDLE_BOX_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_handle_box_get_type';
function  GTK_IS_HANDLE_BOX(obj:pointer):boolean;
function  GTK_IS_HANDLE_BOX_CLASS(klass:pointer):boolean;

function  gtk_handle_box_get_type:TGtkType;cdecl;external gtkdll name 'gtk_handle_box_get_type';
function  gtk_handle_box_new : PGtkWidget;cdecl;external gtkdll name 'gtk_handle_box_new';
{$ifndef gtkwin}
procedure gtk_handle_box_set_shadow_type(handle_box:PGtkHandleBox; thetype:TGtkShadowType);cdecl;external gtkdll name 'gtk_handle_box_set_shadow_type';
procedure gtk_handle_box_set_handle_position(handle_box:PGtkHandleBox; position:TGtkPositionType);cdecl;external gtkdll name 'gtk_handle_box_set_handle_position';
procedure gtk_handle_box_set_snap_edge(handle_box:PGtkHandleBox; edge:TGtkPositionType);cdecl;external gtkdll name 'gtk_handle_box_set_snap_edge';
{$endif}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  handle_position(var a : TGtkHandleBox) : guint;
    begin
       handle_position:=(a.flag0 and bm_TGtkHandleBox_handle_position) shr bp_TGtkHandleBox_handle_position;
    end;

procedure set_handle_position(var a : TGtkHandleBox; __handle_position : guint);
    begin
       a.flag0:=a.flag0 or ((__handle_position shl bp_TGtkHandleBox_handle_position) and bm_TGtkHandleBox_handle_position);
    end;

function  float_window_mapped(var a : TGtkHandleBox) : guint;
    begin
       float_window_mapped:=(a.flag0 and bm_TGtkHandleBox_float_window_mapped) shr bp_TGtkHandleBox_float_window_mapped;
    end;

procedure set_float_window_mapped(var a : TGtkHandleBox; __float_window_mapped : guint);
    begin
       a.flag0:=a.flag0 or ((__float_window_mapped shl bp_TGtkHandleBox_float_window_mapped) and bm_TGtkHandleBox_float_window_mapped);
    end;

function  child_detached(var a : TGtkHandleBox) : guint;
    begin
       child_detached:=(a.flag0 and bm_TGtkHandleBox_child_detached) shr bp_TGtkHandleBox_child_detached;
    end;

procedure set_child_detached(var a : TGtkHandleBox; __child_detached : guint);
    begin
       a.flag0:=a.flag0 or ((__child_detached shl bp_TGtkHandleBox_child_detached) and bm_TGtkHandleBox_child_detached);
    end;

function  in_drag(var a : TGtkHandleBox) : guint;
    begin
       in_drag:=(a.flag0 and bm_TGtkHandleBox_in_drag) shr bp_TGtkHandleBox_in_drag;
    end;

procedure set_in_drag(var a : TGtkHandleBox; __in_drag : guint);
    begin
       a.flag0:=a.flag0 or ((__in_drag shl bp_TGtkHandleBox_in_drag) and bm_TGtkHandleBox_in_drag);
    end;

function  shrink_on_detach(var a : TGtkHandleBox) : guint;
    begin
       shrink_on_detach:=(a.flag0 and bm_TGtkHandleBox_shrink_on_detach) shr bp_TGtkHandleBox_shrink_on_detach;
    end;

procedure set_shrink_on_detach(var a : TGtkHandleBox; __shrink_on_detach : guint);
    begin
       a.flag0:=a.flag0 or ((__shrink_on_detach shl bp_TGtkHandleBox_shrink_on_detach) and bm_TGtkHandleBox_shrink_on_detach);
    end;

function  snap_edge(var a : TGtkHandleBox) : gint;
    begin
       snap_edge:=(a.flag0 and bm_TGtkHandleBox_snap_edge) shr bp_TGtkHandleBox_snap_edge;
    end;

procedure set_snap_edge(var a : TGtkHandleBox; __snap_edge : gint);
    begin
       a.flag0:=a.flag0 or ((__snap_edge shl bp_TGtkHandleBox_snap_edge) and bm_TGtkHandleBox_snap_edge);
    end;

function  GTK_IS_HANDLE_BOX(obj:pointer):boolean;
begin
  GTK_IS_HANDLE_BOX:=(obj<>nil) and GTK_IS_HANDLE_BOX_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_HANDLE_BOX_CLASS(klass:pointer):boolean;
begin
  GTK_IS_HANDLE_BOX_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_HANDLE_BOX_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1.2.1  2000-09-09 18:42:52  peter
    * gtk win32 fixes

  Revision 1.1  2000/07/13 06:34:04  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.10  1999/10/21 08:42:01  florian
    * some changes to get it work with gtk 1.3 under Windows 98:
      - removed some trailing space after the import name
      - In gtkbindings.h is
        #define  gtk_binding_entry_add          gtk_binding_entry_clear
        so in the pascal headers the import name of gtk_bindings_entry_add should be
        gtk_binding_entry_clear!
      - removed the declaration of
        gtk_drag_source_unset in gtkdnd.pp it isn't in gtk-1.3.dll!
      - in gdk.pp glibdll must be set to gdk-1.3:
        const
           gdkdll='gdk-1.3';
           glibdll='gdk-1.3';
        else the whole gdk_* calls are imported from glib-1.3.dll which is wrong!

  Revision 1.9  1999/10/06 17:42:49  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:12:23  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:38:38  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:28  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:15  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:09:55  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:33  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

