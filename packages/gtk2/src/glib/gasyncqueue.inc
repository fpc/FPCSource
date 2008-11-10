// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGAsyncQueue = pointer;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

{ Asyncronous Queues, can be used to communicate between threads }

{* Get a new GAsyncQueue with the ref_count 1 *}

    function g_async_queue_new:PGAsyncQueue;cdecl;external gliblib name 'g_async_queue_new';

{* Lock and unlock an GAsyncQueue, all functions lock the queue for
 * themselves, but in certain cirumstances you want to hold the lock longer,
 * thus you lock the queue, call the *_unlocked functions and unlock it again
 *}
    procedure g_async_queue_lock(queue:PGAsyncQueue);cdecl;external gliblib name 'g_async_queue_lock';
    procedure g_async_queue_unlock(queue:PGAsyncQueue);cdecl;external gliblib name 'g_async_queue_unlock';


{* Ref and unref the GAsyncQueue. g_async_queue_unref_unlocked makes
 * no sense, as after the unreffing the Queue might be gone and can't
 * be unlocked. So you have a function to call, if you don't hold the
 * lock (g_async_queue_unref) and one to call, when you already hold
 * the lock (g_async_queue_unref_and_unlock). After that however, you
 * don't hold the lock anymore and the Queue might in fact be
 * destroyed, if you unrefed to zero *}

    procedure g_async_queue_ref(queue:PGAsyncQueue);cdecl;external gliblib name 'g_async_queue_ref';
    procedure g_async_queue_ref_unlocked(queue:PGAsyncQueue);cdecl;external gliblib name 'g_async_queue_ref_unlocked';
    procedure g_async_queue_unref(queue:PGAsyncQueue);cdecl;external gliblib name 'g_async_queue_unref';
    procedure g_async_queue_unref_and_unlock(queue:PGAsyncQueue);cdecl;external gliblib name 'g_async_queue_unref_and_unlock';

{* Push data into the async queue. Must not be NULL *}
    procedure g_async_queue_push(queue:PGAsyncQueue; data:gpointer);cdecl;external gliblib name 'g_async_queue_push';
    procedure g_async_queue_push_unlocked(queue:PGAsyncQueue; data:gpointer);cdecl;external gliblib name 'g_async_queue_push_unlocked';

{* Pop data from the async queue, when no data is there, the thread is blocked
 * until data arrives *}
    function g_async_queue_pop(queue:PGAsyncQueue):gpointer;cdecl;external gliblib name 'g_async_queue_pop';
    function g_async_queue_pop_unlocked(queue:PGAsyncQueue):gpointer;cdecl;external gliblib name 'g_async_queue_pop_unlocked';

{* Try to pop data, NULL is returned in case of empty queue *}
    function g_async_queue_try_pop(queue:PGAsyncQueue):gpointer;cdecl;external gliblib name 'g_async_queue_try_pop';
    function g_async_queue_try_pop_unlocked(queue:PGAsyncQueue):gpointer;cdecl;external gliblib name 'g_async_queue_try_pop_unlocked';

{* Wait for data until at maximum until end_time is reached, NULL is returned
 * in case of empty queue*}
    function g_async_queue_timed_pop(queue:PGAsyncQueue; end_time:PGTimeVal):gpointer;cdecl;external gliblib name 'g_async_queue_timed_pop';
    function g_async_queue_timed_pop_unlocked(queue:PGAsyncQueue; end_time:PGTimeVal):gpointer;cdecl;external gliblib name 'g_async_queue_timed_pop_unlocked';


{* Return the length of the queue, negative values mean, that threads
 * are waiting, positve values mean, that there are entries in the
 * queue. Actually this function returns the length of the queue minus
 * the number of waiting threads, g_async_queue_length == 0 could also
 * mean 'n' entries in the queue and 'n' thread waiting, such can
 * happen due to locking of the queue or due to scheduling. *}

    function g_async_queue_length(queue:PGAsyncQueue):gint;cdecl;external gliblib name 'g_async_queue_length';
    function g_async_queue_length_unlocked(queue:PGAsyncQueue):gint;cdecl;external gliblib name 'g_async_queue_length_unlocked';

{$ENDIF read_interface_rest}
