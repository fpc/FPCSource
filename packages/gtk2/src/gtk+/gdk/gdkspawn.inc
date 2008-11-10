// included by gdk2.pp

{$IFDEF read_interface_rest}
function gdk_spawn_on_screen(screen: PGdkScreen; working_directory: Pgchar; argv: PPgchar; envp: PPgchar;
           flags: TGSpawnFlags; child_setup: TGSpawnChildSetupFunc; user_data: gpointer; child_pid: Pgint;
           error: PPGError): gboolean; cdecl; external gdklib;

function gdk_spawn_on_screen_with_pipes(screen: PGdkScreen; working_directory: Pgchar; argv: PPgchar; envp: PPgchar;
           flags: TGSpawnFlags; child_setup: TGSpawnChildSetupFunc; user_data: gpointer; child_pid: Pgint;
           standard_input: Pgint; standard_output: Pgint; standard_error: Pgint; error: PPGError): gboolean; cdecl; external gdklib;

function gdk_spawn_command_line_on_screen(screen: PGdkScreen; command_line: Pgchar; error: PPGError): gboolean; cdecl; external gdklib;
{$endif read_interface_rest}
