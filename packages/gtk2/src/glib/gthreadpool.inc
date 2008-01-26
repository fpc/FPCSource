// included by glib2.pas

type
{ Thread Pools }

{ The real GThreadPool is bigger, so you may only create a thread
   pool with the constructor function  }
   PGThreadPool = ^TGThreadPool;
   TGThreadPool = record
        func : TGFunc;
        user_data : gpointer;
        exclusive : gboolean;
     end;

{ Get a thread pool with the function func, at most max_threads may
   run at a time (max_threads == -1 means no limit), exclusive == TRUE
   means, that the threads shouldn't be shared and that they will be
   prestarted (otherwise they are started as needed) user_data is the
   2nd argument to the func  }
function g_thread_pool_new(func:TGFunc; user_data:gpointer; max_threads:gint; exclusive:gboolean; error:PPGError):PGThreadPool; cdecl; external gliblib;

{ Push new data into the thread pool. This task is assigned to a thread later
   (when the maximal number of threads is reached for that pool) or now
   (otherwise). If necessary a new thread will be started. The function
   returns immediatly  }
procedure g_thread_pool_push(pool:PGThreadPool; data:gpointer; error:PPGError); cdecl; external gliblib;

{ Set the number of threads, which can run concurrently for that pool, -1
   means no limit. 0 means has the effect, that the pool won't process
   requests until the limit is set higher again  }
procedure g_thread_pool_set_max_threads(pool:PGThreadPool; max_threads:gint; error:PPGError); cdecl; external gliblib;
function g_thread_pool_get_max_threads(pool:PGThreadPool):gint; cdecl; external gliblib;

{ Get the number of threads assigned to that pool. This number doesn't
   necessarily represent the number of working threads in that pool  }
function g_thread_pool_get_num_threads(pool:PGThreadPool):guint; cdecl; external gliblib;

{ Get the number of unprocessed items in the pool  }
function g_thread_pool_unprocessed(pool:PGThreadPool):guint; cdecl; external gliblib;

{ Free the pool, immediate means, that all unprocessed items in the queue
   wont be processed, wait means, that the function doesn't return immediatly,
   but after all threads in the pool are ready processing items. immediate
   does however not mean, that threads are killed.  }
procedure g_thread_pool_free(pool:PGThreadPool; immediate:gboolean; wait:gboolean); cdecl; external gliblib;

{ Set the maximal number of unused threads before threads will be stopped by
   GLib, -1 means no limit  }
procedure g_thread_pool_set_max_unused_threads(max_threads:gint); cdecl; external gliblib;
function g_thread_pool_get_max_unused_threads:gint; cdecl; external gliblib;
function g_thread_pool_get_num_unused_threads:guint; cdecl; external gliblib;

{ Stop all currently unused threads, but leave the limit untouched  }
procedure g_thread_pool_stop_unused_threads; cdecl; external gliblib;

// included by glib2.pas

