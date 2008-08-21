// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGSpawnError = ^TGSpawnError;
  TGSpawnError = (
    G_SPAWN_ERROR_FORK,
    G_SPAWN_ERROR_READ,
    G_SPAWN_ERROR_CHDIR,
    G_SPAWN_ERROR_ACCES,
    G_SPAWN_ERROR_PERM,
    G_SPAWN_ERROR_2BIG,
    G_SPAWN_ERROR_NOEXEC,
    G_SPAWN_ERROR_NAMETOOLONG,
    G_SPAWN_ERROR_NOENT,
    G_SPAWN_ERROR_NOMEM,
    G_SPAWN_ERROR_NOTDIR,
    G_SPAWN_ERROR_LOOP,
    G_SPAWN_ERROR_TXTBUSY,
    G_SPAWN_ERROR_IO,
    G_SPAWN_ERROR_NFILE,
    G_SPAWN_ERROR_MFILE,
    G_SPAWN_ERROR_INVAL,
    G_SPAWN_ERROR_ISDIR,
    G_SPAWN_ERROR_LIBBAD,
    G_SPAWN_ERROR_FAILED
  );

  TGSpawnChildSetupFunc = procedure (user_data:gpointer);cdecl;

  PGSpawnFlags = ^TGSpawnFlags;
  TGSpawnFlags = integer;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
  G_SPAWN_LEAVE_DESCRIPTORS_OPEN = 1 shl 0;
  G_SPAWN_DO_NOT_REAP_CHILD = 1 shl 1;
  G_SPAWN_SEARCH_PATH = 1 shl 2;
  G_SPAWN_STDOUT_TO_DEV_NULL = 1 shl 3;
  G_SPAWN_STDERR_TO_DEV_NULL = 1 shl 4;
  G_SPAWN_CHILD_INHERITS_STDIN = 1 shl 5;
  G_SPAWN_FILE_AND_ARGV_ZERO = 1 shl 6;

function g_spawn_error_quark:TGQuark;cdecl;external gliblib name 'g_spawn_error_quark';
function g_spawn_async(working_directory:Pgchar; argv:PPgchar; envp:PPgchar; flags:TGSpawnFlags; child_setup:TGSpawnChildSetupFunc;
           user_data:gpointer; child_pid:Pgint; error:PPGError):gboolean;cdecl;external gliblib name 'g_spawn_async';

function g_spawn_async_with_pipes(working_directory:Pgchar; argv:PPgchar; envp:PPgchar; flags:TGSpawnFlags; child_setup:TGSpawnChildSetupFunc;
           user_data:gpointer; child_pid:Pgint; standard_input:Pgint; standard_output:Pgint; standard_error:Pgint;
           error:PPGError):gboolean;cdecl;external gliblib name 'g_spawn_async_with_pipes';

function g_spawn_sync(working_directory:Pgchar; argv:PPgchar; envp:PPgchar; flags:TGSpawnFlags; child_setup:TGSpawnChildSetupFunc;
           user_data:gpointer; standard_output:PPgchar; standard_error:PPgchar; exit_status:Pgint; error:PPGError):gboolean;cdecl;external gliblib name 'g_spawn_sync';

function g_spawn_command_line_sync(command_line:Pgchar; standard_output:PPgchar; standard_error:PPgchar; exit_status:Pgint; error:PPGError):gboolean;cdecl;external gliblib name 'g_spawn_command_line_sync';
function g_spawn_command_line_async(command_line:Pgchar; error:PPGError):gboolean;cdecl;external gliblib name 'g_spawn_command_line_async';
{$ENDIF read_interface_rest}
// included by glib2.pas
