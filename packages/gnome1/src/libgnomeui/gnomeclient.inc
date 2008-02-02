{$IfDef read_interface}

type
   TGnomeInteractStyle = (GNOME_INTERACT_NONE,GNOME_INTERACT_ERRORS,
     GNOME_INTERACT_ANY);

   TGnomeDialogType = (GNOME_DIALOG_ERROR,GNOME_DIALOG_NORMAL
     );

   TGnomeSaveStyle = (GNOME_SAVE_GLOBAL,GNOME_SAVE_LOCAL,GNOME_SAVE_BOTH
     );

   TGnomeRestartStyle = (GNOME_RESTART_IF_RUNNING,GNOME_RESTART_ANYWAY,
     GNOME_RESTART_IMMEDIATELY,GNOME_RESTART_NEVER
     );

   TGnomeClientState = (GNOME_CLIENT_IDLE,GNOME_CLIENT_SAVING_PHASE_1,
     GNOME_CLIENT_WAITING_FOR_PHASE_2,GNOME_CLIENT_SAVING_PHASE_2,
     GNOME_CLIENT_FROZEN,GNOME_CLIENT_DISCONNECTED,
     GNOME_CLIENT_REGISTERING);

   TGnomeClientFlags = (GNOME_CLIENT_IS_CONNECTED := 1 shl 0,GNOME_CLIENT_RESTARTED := 1 shl 1,
     GNOME_CLIENT_RESTORED := 1 shl 2);

   PGnomeClient = ^TGnomeClient;
   TGnomeClient = record
        theobject : TGtkObject;
        smc_conn : gpointer;
        input_id : gint;
        client_id : Pgchar;
        previous_id : Pgchar;
        config_prefix : Pgchar;
        global_config_prefix : Pgchar;
        static_args : PGList;
        clone_command : PPgchar;
        current_directory : Pgchar;
        discard_command : PPgchar;
        environment : PGHashTable;
        process_id : pid_t;
        theprogram : Pgchar;
        resign_command : PPgchar;
        restart_command : PPgchar;
        restart_style : TGnomeRestartStyle;
        shutdown_command : PPgchar;
        user_id : Pgchar;
        save_style : TGnomeSaveStyle;
        interact_style : TGnomeInteractStyle;
        shutdown : gboolean;
        fast : gboolean;
        state : TGnomeClientState;
        save_phase_2_requested : gboolean;
        save_successfull : gboolean;
        save_yourself_emitted : gboolean;
        interaction_keys : PGSList;
     end;
   GNOME_CLIENT = PGnomeClient;

   TGnomeInteractFunction = procedure (client:PGnomeClient; key:gint; dialog_type:TGnomeDialogType; data:gpointer);cdecl;

  function GNOME_CLIENT_CONNECTED(obj : pointer) : gboolean;

Type
   PGnomeClientClass = ^TGnomeClientClass;
   TGnomeClientClass = record
        parent_class : TGtkObjectClass;
        save_yourself : function (client:PGnomeClient; phase:gint; save_style:TGnomeSaveStyle; shutdown:gint; interact_style:TGnomeInteractStyle;
                     fast:gint):gboolean; cdecl;
        die : procedure (client:PGnomeClient); cdecl;
        save_complete : procedure (client:PGnomeClient); cdecl;
        shutdown_cancelled : procedure (client:PGnomeClient); cdecl;
        connect : procedure (client:PGnomeClient; restarted:gint); cdecl;
        disconnect : procedure (client:PGnomeClient);cdecl;
     end;
   GNOME_CLIENT_CLASS = PGnomeClientClass;

function GNOME_TYPE_CLIENT : TGTKType;
function GNOME_IS_CLIENT(obj : Pointer) : Boolean;
function GNOME_IS_CLIENT_CLASS(klass : Pointer) : Boolean;

function gnome_client_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_client_get_type';
procedure gnome_client_init;cdecl;external libgnomeuidll name 'gnome_client_init';
function gnome_master_client:PGnomeClient;cdecl;external libgnomeuidll name 'gnome_master_client';
function gnome_client_get_config_prefix(client:PGnomeClient):Pgchar;cdecl;external libgnomeuidll name 'gnome_client_get_config_prefix';
function gnome_client_get_global_config_prefix(client:PGnomeClient):Pgchar;cdecl;external libgnomeuidll name 'gnome_client_get_global_config_prefix';
procedure gnome_client_set_global_config_prefix(client:PGnomeClient; prefix:Pgchar);cdecl;external libgnomeuidll name 'gnome_client_set_global_config_prefix';
function gnome_client_get_flags(client:PGnomeClient):TGnomeClientFlags;cdecl;external libgnomeuidll name 'gnome_client_get_flags';
procedure gnome_client_set_restart_style(client:PGnomeClient; style:TGnomeRestartStyle);cdecl;external libgnomeuidll name 'gnome_client_set_restart_style';
procedure gnome_client_set_priority(client:PGnomeClient; priority:guint);cdecl;external libgnomeuidll name 'gnome_client_set_priority';
procedure gnome_client_set_restart_command(client:PGnomeClient; argc:gint; argv:PPgchar);cdecl;external libgnomeuidll name 'gnome_client_set_restart_command';
procedure gnome_client_add_static_arg(client:PGnomeClient; args:array of const);cdecl;external libgnomeuidll name 'gnome_client_add_static_arg';
procedure gnome_client_set_discard_command(client:PGnomeClient; argc:gint; argv:PPgchar);cdecl;external libgnomeuidll name 'gnome_client_set_discard_command';
procedure gnome_client_set_resign_command(client:PGnomeClient; argc:gint; argv:PPgchar);cdecl;external libgnomeuidll name 'gnome_client_set_resign_command';
procedure gnome_client_set_shutdown_command(client:PGnomeClient; argc:gint; argv:PPgchar);cdecl;external libgnomeuidll name 'gnome_client_set_shutdown_command';
procedure gnome_client_set_current_directory(client:PGnomeClient; dir:Pgchar);cdecl;external libgnomeuidll name 'gnome_client_set_current_directory';
procedure gnome_client_set_environment(client:PGnomeClient; name:Pgchar; value:Pgchar);cdecl;external libgnomeuidll name 'gnome_client_set_environment';
procedure gnome_client_set_clone_command(client:PGnomeClient; argc:gint; argv:PPgchar);cdecl;external libgnomeuidll name 'gnome_client_set_clone_command';
procedure gnome_client_set_process_id(client:PGnomeClient; pid:pid_t);cdecl;external libgnomeuidll name 'gnome_client_set_process_id';
procedure gnome_client_set_program(client:PGnomeClient; theprogram:Pgchar);cdecl;external libgnomeuidll name 'gnome_client_set_program';
procedure gnome_client_set_user_id(client:PGnomeClient; user_id:Pgchar);cdecl;external libgnomeuidll name 'gnome_client_set_user_id';
procedure gnome_client_save_any_dialog(client:PGnomeClient; dialog:PGnomeDialog);cdecl;external libgnomeuidll name 'gnome_client_save_any_dialog';
procedure gnome_client_save_error_dialog(client:PGnomeClient; dialog:PGnomeDialog);cdecl;external libgnomeuidll name 'gnome_client_save_error_dialog';
procedure gnome_client_request_phase_2(client:PGnomeClient);cdecl;external libgnomeuidll name 'gnome_client_request_phase_2';
procedure gnome_client_request_save(client:PGnomeClient; save_style:TGnomeSaveStyle; shutdown:gboolean; interact_style:TGnomeInteractStyle; fast:gboolean;
            global:gboolean);cdecl;external libgnomeuidll name 'gnome_client_request_save';
procedure gnome_client_flush(client:PGnomeClient);cdecl;external libgnomeuidll name 'gnome_client_flush';
procedure gnome_client_disable_master_connection;cdecl;external libgnomeuidll name 'gnome_client_disable_master_connection';
function gnome_client_new:PGnomeClient;cdecl;external libgnomeuidll name 'gnome_client_new';
function gnome_client_new_without_connection:PGnomeClient;cdecl;external libgnomeuidll name 'gnome_client_new_without_connection';
procedure gnome_client_connect(client:PGnomeClient);cdecl;external libgnomeuidll name 'gnome_client_connect';
procedure gnome_client_disconnect(client:PGnomeClient);cdecl;external libgnomeuidll name 'gnome_client_disconnect';
procedure gnome_client_set_id(client:PGnomeClient; client_id:Pgchar);cdecl;external libgnomeuidll name 'gnome_client_set_id';
function gnome_client_get_id(client:PGnomeClient):Pgchar;cdecl;external libgnomeuidll name 'gnome_client_get_id';
function gnome_client_get_previous_id(client:PGnomeClient):Pgchar;cdecl;external libgnomeuidll name 'gnome_client_get_previous_id';
function gnome_cloned_client:PGnomeClient;cdecl;external libgnomeuidll name 'gnome_cloned_client';
procedure gnome_client_request_interaction(client:PGnomeClient; dialog:TGnomeDialogType; func:TGnomeInteractFunction; client_data:gpointer);cdecl;external libgnomeuidll name 'gnome_client_request_interaction';
procedure gnome_client_request_interaction_interp(client:PGnomeClient; dialog:TGnomeDialogType; func:TGtkCallbackMarshal; data:gpointer; destroy:TGtkDestroyNotify);cdecl;external libgnomeuidll name 'gnome_client_request_interaction_interp';
procedure gnome_interaction_key_return(key:gint; cancel_shutdown:gboolean);cdecl;external libgnomeuidll name 'gnome_interaction_key_return';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_CLIENT : TGTKType;
begin
  GNOME_TYPE_CLIENT:=gnome_client_get_type;
end;

function GNOME_IS_CLIENT(obj : Pointer) : Boolean;
begin
   GNOME_IS_CLIENT:=(obj<>nil) and GNOME_IS_CLIENT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CLIENT_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CLIENT_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CLIENT);
end;

function GNOME_CLIENT_CONNECTED(obj : pointer) : gboolean;
begin
   GNOME_CLIENT_CONNECTED:=(obj <> nil) and ((GNOME_CLIENT(obj))^.smc_conn <> nil);
end;

{$Endif read_implementation}
