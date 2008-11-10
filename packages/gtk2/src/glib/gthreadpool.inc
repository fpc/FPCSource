// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGThreadPool = ^TGThreadPool;
  TGThreadPool = record
    func : TGFunc;
    user_data : gpointer;
    exclusive : gboolean;
  end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function g_thread_pool_new(func:TGFunc; user_data:gpointer; max_threads:gint; exclusive:gboolean; error:PPGError):PGThreadPool; cdecl; external gliblib;
procedure g_thread_pool_push(pool:PGThreadPool; data:gpointer; error:PPGError); cdecl; external gliblib;
procedure g_thread_pool_set_max_threads(pool:PGThreadPool; max_threads:gint; error:PPGError); cdecl; external gliblib;
function g_thread_pool_get_max_threads(pool:PGThreadPool):gint; cdecl; external gliblib;
function g_thread_pool_get_num_threads(pool:PGThreadPool):guint; cdecl; external gliblib;
function g_thread_pool_unprocessed(pool:PGThreadPool):guint; cdecl; external gliblib;
procedure g_thread_pool_free(pool:PGThreadPool; immediate:gboolean; wait:gboolean); cdecl; external gliblib;
procedure g_thread_pool_set_max_unused_threads(max_threads:gint); cdecl; external gliblib;
function g_thread_pool_get_max_unused_threads:gint; cdecl; external gliblib;
function g_thread_pool_get_num_unused_threads:guint; cdecl; external gliblib;
procedure g_thread_pool_stop_unused_threads; cdecl; external gliblib;
{$ENDIF read_interface_rest}

// included by glib2.pas

