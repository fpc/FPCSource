{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkContainer = ^TGtkContainer;
       TGtkContainer = record
            widget : TGtkWidget;
            focus_child : PGtkWidget;
            flag0 : longint;
            resize_widgets : PGSList;
         end;
  const
     bm_TGtkContainer_border_width = $FFFF;
     bp_TGtkContainer_border_width = 0;
     bm_TGtkContainer_need_resize = $10000;
     bp_TGtkContainer_need_resize = 16;
     bm_TGtkContainer_resize_mode = $60000;
     bp_TGtkContainer_resize_mode = 17;
function  border_width(var a : TGtkContainer) : guint;
procedure set_border_width(var a : TGtkContainer; __border_width : guint);
function  need_resize(var a : TGtkContainer) : guint;
procedure set_need_resize(var a : TGtkContainer; __need_resize : guint);
function  resize_mode(var a : TGtkContainer) : guint;
procedure set_resize_mode(var a : TGtkContainer; __resize_mode : guint);

    type
       PGtkContainerClass = ^TGtkContainerClass;
       TGtkContainerClass = record
          parent_class : TGtkWidgetClass;
          n_child_args : guint;
          add : procedure (container:PGtkContainer; widget:PGtkWidget); cdecl;
          remove : procedure (container:PGtkContainer; widget:PGtkWidget); cdecl;
          check_resize : procedure (container:PGtkContainer);cdecl;
          forall : procedure (container:PGtkContainer; include_internals:gboolean; callback:TGtkCallback; callbabck_data:gpointer);cdecl;
          focus : function (container:PGtkContainer; direction:TGtkDirectionType):gint; cdecl;
          set_focus_child : procedure (container:PGtkContainer; widget:PGtkWidget);cdecl;
          child_type : function (container:PGtkContainer):TGtkType;cdecl;
          set_child_arg : procedure (container:PGtkContainer; child:PGtkWidget; arg:PGtkArg; arg_id:guint);cdecl;
          get_child_arg : procedure (container:PGtkContainer; child:PGtkWidget; arg:PGtkArg; arg_id:guint);cdecl;
          composite_name : function (container:PGtkContainer; child:PGtkWidget):Pgchar;cdecl;
          pad1 : TGtkfunction;
          pad2 : TGtkfunction;
        end;

Type
  GTK_CONTAINER = PGtkContainer;
  GTK_CONTAINER_CLASS = PGtkContainerClass;

function  GTK_CONTAINER_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_container_get_type';
function  GTK_IS_CONTAINER(obj:pointer):boolean;
function  GTK_IS_CONTAINER_CLASS(klass:pointer):boolean;

function  gtk_container_get_type:TGtkType;cdecl;external gtkdll name 'gtk_container_get_type';
procedure gtk_container_set_border_width(container:PGtkContainer; border_width:guint);cdecl;external gtkdll name 'gtk_container_set_border_width';
procedure gtk_container_add(container:PGtkContainer; widget:PGtkWidget);cdecl;external gtkdll name 'gtk_container_add';
procedure gtk_container_remove(container:PGtkContainer; widget:PGtkWidget);cdecl;external gtkdll name 'gtk_container_remove';
procedure gtk_container_set_resize_mode(container:PGtkContainer; resize_mode:TGtkResizeMode);cdecl;external gtkdll name 'gtk_container_set_resize_mode';
procedure gtk_container_check_resize(container:PGtkContainer);cdecl;external gtkdll name 'gtk_container_check_resize';
procedure gtk_container_foreach(container:PGtkContainer; callback:TGtkCallback; callback_data:gpointer);cdecl;external gtkdll name 'gtk_container_foreach';
procedure gtk_container_foreach_full(container:PGtkContainer; callback:TGtkCallback; marshal:TGtkCallbackMarshal; callback_data:gpointer; notify:TGtkDestroyNotify);cdecl;external gtkdll name 'gtk_container_foreach_full';
function  gtk_container_children (container:PGtkContainer):PGList;cdecl;external gtkdll name 'gtk_container_children';
function  gtk_container_focus(container:PGtkContainer; direction:TGtkDirectionType):gint;cdecl;external gtkdll name 'gtk_container_focus';
procedure gtk_container_set_focus_child(container:PGtkContainer; child:PGtkWidget);cdecl;external gtkdll name 'gtk_container_set_focus_child';
procedure gtk_container_set_focus_vadjustment(container:PGtkContainer; adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_container_set_focus_vadjustment';
procedure gtk_container_set_focus_hadjustment(container:PGtkContainer; adjustment:PGtkAdjustment);cdecl;external gtkdll name 'gtk_container_set_focus_hadjustment';
procedure gtk_container_register_toplevel(container:PGtkContainer);cdecl;external gtkdll name 'gtk_container_register_toplevel';
procedure gtk_container_unregister_toplevel(container:PGtkContainer);cdecl;external gtkdll name 'gtk_container_unregister_toplevel';
{$ifndef gtkwin}
function  gtk_container_get_toplevels:PGList;cdecl;external gtkdll name 'gtk_container_get_toplevels';
{$endif}
procedure gtk_container_resize_children(container:PGtkContainer);cdecl;external gtkdll name 'gtk_container_resize_children';
function  gtk_container_child_type(container:PGtkContainer):TGtkType;cdecl;external gtkdll name 'gtk_container_child_type';
procedure gtk_container_add_child_arg_type(arg_name:Pgchar; arg_type:TGtkType; arg_flags:guint; arg_id:guint);cdecl;external gtkdll name 'gtk_container_add_child_arg_type';
function  gtk_container_query_child_args(class_type:TGtkType; arg_flags:PPguint32; nargs:Pguint):PGtkArg;cdecl;external gtkdll name 'gtk_container_query_child_args';
procedure gtk_container_child_getv(container:PGtkContainer; child:PGtkWidget; n_args:guint; args:PGtkArg);cdecl;external gtkdll name 'gtk_container_child_getv';
procedure gtk_container_child_setv(container:PGtkContainer; child:PGtkWidget; n_args:guint; args:PGtkArg);cdecl;external gtkdll name 'gtk_container_child_setv';
procedure gtk_container_add_with_args(container:PGtkContainer; widget:PGtkWidget; first_arg_name:Pgchar; args:array of const);cdecl;external gtkdll name 'gtk_container_add_with_args';
procedure gtk_container_addv(container:PGtkContainer; widget:PGtkWidget; n_args:guint; args:PGtkArg);cdecl;external gtkdll name 'gtk_container_addv';
procedure gtk_container_child_set(container:PGtkContainer; child:PGtkWidget; first_arg_name:Pgchar; args:array of const);cdecl;external gtkdll name 'gtk_container_child_set';
procedure gtk_container_queue_resize(container:PGtkContainer);cdecl;external gtkdll name 'gtk_container_queue_resize';
{$ifndef gtkwin}
procedure gtk_container_clear_resize_widgets(container:PGtkContainer);cdecl;external gtkdll name 'gtk_container_clear_resize_widgets';
procedure gtk_container_arg_set(container:PGtkContainer; child:PGtkWidget; arg:PGtkArg; info:PGtkArgInfo);cdecl;external gtkdll name 'gtk_container_arg_set';
procedure gtk_container_arg_get(container:PGtkContainer; child:PGtkWidget; arg:PGtkArg; info:PGtkArgInfo);cdecl;external gtkdll name 'gtk_container_arg_get';
function  gtk_container_child_args_collect(object_type:TGtkType; arg_list_p:PPGSList; info_list_p:PPGSList; first_arg_name:Pgchar; args:array of const):Pgchar;cdecl;external gtkdll name 'gtk_container_child_args_collect';
function  gtk_container_child_arg_get_info(object_type:TGtkType; arg_name:Pgchar; info_p:PPGtkArgInfo):Pgchar;cdecl;external gtkdll name 'gtk_container_child_arg_get_info';
procedure gtk_container_forall(container:PGtkContainer; callback:TGtkCallback; callback_data:gpointer);cdecl;external gtkdll name 'gtk_container_forall';
function  gtk_container_child_composite_name(container:PGtkContainer; child:PGtkWidget):Pgchar;cdecl;external gtkdll name 'gtk_container_child_composite_name';
{$endif}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  border_width(var a : TGtkContainer) : guint;
    begin
       border_width:=(a.flag0 and bm_TGtkContainer_border_width) shr bp_TGtkContainer_border_width;
    end;

procedure set_border_width(var a : TGtkContainer; __border_width : guint);
    begin
       a.flag0:=a.flag0 or ((__border_width shl bp_TGtkContainer_border_width) and bm_TGtkContainer_border_width);
    end;

function  need_resize(var a : TGtkContainer) : guint;
    begin
       need_resize:=(a.flag0 and bm_TGtkContainer_need_resize) shr bp_TGtkContainer_need_resize;
    end;

procedure set_need_resize(var a : TGtkContainer; __need_resize : guint);
    begin
       a.flag0:=a.flag0 or ((__need_resize shl bp_TGtkContainer_need_resize) and bm_TGtkContainer_need_resize);
    end;

function  resize_mode(var a : TGtkContainer) : guint;
    begin
       resize_mode:=(a.flag0 and bm_TGtkContainer_resize_mode) shr bp_TGtkContainer_resize_mode;
    end;

procedure set_resize_mode(var a : TGtkContainer; __resize_mode : guint);
    begin
       a.flag0:=a.flag0 or ((__resize_mode shl bp_TGtkContainer_resize_mode) and bm_TGtkContainer_resize_mode);
    end;

function  GTK_IS_CONTAINER(obj:pointer):boolean;
begin
  GTK_IS_CONTAINER:=(obj<>nil) and GTK_IS_CONTAINER_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_CONTAINER_CLASS(klass:pointer):boolean;
begin
  GTK_IS_CONTAINER_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_CONTAINER_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:03  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.13  1999/10/21 08:42:01  florian
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

  Revision 1.12  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.11  1999/07/23 16:12:09  peter
    * use packrecords C

  Revision 1.10  1999/05/11 00:38:20  peter
    * win32 fixes

  Revision 1.9  1999/05/10 19:18:28  peter
    * more fixes for the examples to work

  Revision 1.8  1999/05/10 15:19:08  peter
    * cdecl fixes

  Revision 1.7  1999/05/10 09:03:03  peter
    * gtk 1.2 port working

  Revision 1.6  1999/05/07 15:09:57  peter
    * more fixes

  Revision 1.5  1999/05/07 10:40:34  peter
    * first things for 1.2

  Revision 1.4  1998/11/09 10:09:43  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:18  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

