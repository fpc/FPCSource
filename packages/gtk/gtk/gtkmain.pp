{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  const
     GTK_PRIORITY_REDRAW = G_PRIORITY_HIGH_IDLE + 20;
     GTK_PRIORITY_RESIZE = G_PRIORITY_HIGH_IDLE + 10;
     GTK_PRIORITY_HIGH = G_PRIORITY_HIGH;
     GTK_PRIORITY_INTERNAL = GTK_PRIORITY_REDRAW;
     GTK_PRIORITY_DEFAULT = G_PRIORITY_DEFAULT_IDLE;
     GTK_PRIORITY_LOW = G_PRIORITY_LOW;

  type

     TGtkModuleInitFunc = procedure (argc:Pgint; argv:pPPgchar);cdecl;
     TGtkKeySnoopFunc = function (grab_widget:PGtkWidget;event:PGdkEventKey;func_data:gpointer):gint;cdecl;

    var
       gtk_major_version : guint;external gtkdll name 'gtk_major_version';
       gtk_minor_version : guint;external gtkdll name 'gtk_minor_version';
       gtk_micro_version : guint;external gtkdll name 'gtk_micro_version';
       gtk_binary_age : guint;external gtkdll name 'gtk_binary_age';
       gtk_interface_age : guint;external gtkdll name 'gtk_interface_age';

function  gtk_check_gtkversion(required_major:guint; required_minor:guint; required_micro:guint):Pgchar;cdecl;external gtkdll name 'gtk_check_version';
procedure gtk_init(argc:plongint; argv:pppchar);cdecl;external gtkdll name 'gtk_init';
function  gtk_init_check(argc:plongint; argv:pppchar):gboolean;cdecl;external gtkdll name 'gtk_init_check';
procedure gtk_exit(error_code:gint);cdecl;external gtkdll name 'gtk_exit';
function  gtk_set_locale:Pgchar;cdecl;external gtkdll name 'gtk_set_locale';
function  gtk_events_pending:gint;cdecl;external gtkdll name 'gtk_events_pending';
procedure gtk_main_do_event(event:PGdkEvent);cdecl;external gtkdll name 'gtk_main_do_event';
procedure gtk_main;cdecl;external gtkdll name 'gtk_main';
function  gtk_main_level:guint;cdecl;external gtkdll name 'gtk_main_level';
procedure gtk_main_quit;cdecl;external gtkdll name 'gtk_main_quit';
function  gtk_main_iteration:gint;cdecl;external gtkdll name 'gtk_main_iteration';
function  gtk_main_iteration_do(blocking:gboolean):gint;cdecl;external gtkdll name 'gtk_main_iteration_do';
function  gtk_true:gint;cdecl;external gtkdll name 'gtk_true';
function  gtk_false:gint;cdecl;external gtkdll name 'gtk_false';
procedure gtk_grab_add(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_grab_add';
function  gtk_grab_get_current:PGtkWidget;cdecl;external gtkdll name 'gtk_grab_get_current';
procedure gtk_grab_remove(widget:PGtkWidget);cdecl;external gtkdll name 'gtk_grab_remove';
procedure gtk_init_add(thefunction :TGtkfunction; data:gpointer);cdecl;external gtkdll name 'gtk_init_add';
procedure gtk_quit_add_destroy(main_level:guint; theobject:PGtkObject);cdecl;external gtkdll name 'gtk_quit_add_destroy';
function  gtk_quit_add(main_level:guint; thefunction:TGtkfunction; data:gpointer):guint;cdecl;external gtkdll name 'gtk_quit_add';
function  gtk_quit_add_full(main_level:guint; thefunction:TGtkfunction; marshal:TGtkCallbackMarshal; data:gpointer; destroy:TGtkDestroyNotify):guint;cdecl;external gtkdll name 'gtk_quit_add_full';
procedure gtk_quit_remove(quit_handler_id:guint);cdecl;external gtkdll name 'gtk_quit_remove';
procedure gtk_quit_remove_by_data(data:gpointer);cdecl;external gtkdll name 'gtk_quit_remove_by_data';
function  gtk_timeout_add(interval:guint32; thefunction:TGtkfunction; data:gpointer):guint;cdecl;external gtkdll name 'gtk_timeout_add';
function  gtk_timeout_add_full(interval:guint32; thefunction:TGtkfunction; marshal:TGtkCallbackMarshal; data:gpointer; destroy:TGtkDestroyNotify):guint;cdecl;external gtkdll name 'gtk_timeout_add_full';
procedure gtk_timeout_remove(timeout_handler_id:guint);cdecl;external gtkdll name 'gtk_timeout_remove';
function  gtk_idle_add(thefunction:TGtkfunction; data:gpointer):guint;cdecl;external gtkdll name 'gtk_idle_add';
function  gtk_idle_add_priority(priority:gint; thefunction:TGtkfunction; data:gpointer):guint;cdecl;external gtkdll name 'gtk_idle_add_priority';
function  gtk_idle_add_full(priority:gint; thefunction:TGtkfunction; marshal:TGtkCallbackMarshal; data:gpointer; destroy:TGtkDestroyNotify):guint;cdecl;external gtkdll name 'gtk_idle_add_full';
procedure gtk_idle_remove(idle_handler_id:guint);cdecl;external gtkdll name 'gtk_idle_remove';
procedure gtk_idle_remove_by_data(data:gpointer);cdecl;external gtkdll name 'gtk_idle_remove_by_data';
function  gtk_input_add_full(source:gint; condition:TGdkInputCondition; thefunction:TGdkInputfunction; marshal:TGtkCallbackMarshal; data:gpointer; destroy:TGtkDestroyNotify):guint;cdecl;external gtkdll name 'gtk_input_add_full';
procedure gtk_input_remove(input_handler_id:guint);cdecl;external gtkdll name 'gtk_input_remove';
function  gtk_key_snooper_install(snooper:TGtkKeySnoopFunc; func_data:gpointer):guint;cdecl;external gtkdll name 'gtk_key_snooper_install';
procedure gtk_key_snooper_remove(snooper_handler_id:guint);cdecl;external gtkdll name 'gtk_key_snooper_remove';
function  gtk_get_current_event:PGdkEvent;cdecl;external gtkdll name 'gtk_get_current_event';
function  gtk_get_event_widget(event:PGdkEvent):PGtkWidget;cdecl;external gtkdll name 'gtk_get_event_widget';
{$ifndef gtkwin}
procedure gtk_propagate_event(widget:PGtkWidget; event:PGdkEvent);cdecl;external gtkdll name 'gtk_propagate_event';
{$endif}

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}
{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.2  2000/02/14 19:18:58  peter
    * win32 updates from vincent snijder

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:49  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/05/11 00:38:55  peter
    * win32 fixes

  Revision 1.7  1999/05/10 15:19:47  peter
    * cdecl fixes

  Revision 1.6  1999/05/10 09:03:21  peter
    * gtk 1.2 port working

  Revision 1.5  1999/05/07 15:10:09  peter
    * more fixes

  Revision 1.4  1998/11/09 10:10:09  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:47  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

