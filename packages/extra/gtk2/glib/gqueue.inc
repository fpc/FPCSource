{*
 * gqueue.inc
 *
 * depends on glist.inc
 *}

  type
     PGQueue = ^TGQueue;
     TGQueue = record
          head   : PGList;
          tail   : PGList;
          length : guint;
       end;

  { Queues }

  function  g_queue_new:PGQueue;cdecl;external gliblib name 'g_queue_new';

  procedure g_queue_free(queue:PGQueue);cdecl;external gliblib name 'g_queue_free';

  procedure g_queue_push_head(queue:PGQueue; data:gpointer);cdecl;external gliblib name 'g_queue_push_head';

  procedure g_queue_push_tail(queue:PGQueue; data:gpointer);cdecl;external gliblib name 'g_queue_push_tail';

  function  g_queue_pop_head(queue:PGQueue):gpointer;cdecl;external gliblib name 'g_queue_pop_head';

  function  g_queue_pop_tail(queue:PGQueue):gpointer;cdecl;external gliblib name 'g_queue_pop_tail';

  function  g_queue_is_empty(queue:PGQueue):gboolean;cdecl;external gliblib name 'g_queue_is_empty';

  function  g_queue_peek_head(queue:PGQueue):gpointer;cdecl;external gliblib name 'g_queue_peek_head';

  function  g_queue_peek_tail(queue:PGQueue):gpointer;cdecl;external gliblib name 'g_queue_peek_tail';

  procedure g_queue_push_head_link(queue:PGQueue; link:PGList);cdecl;external gliblib name 'g_queue_push_head_link';

  procedure g_queue_push_tail_link(queue:PGQueue; link:PGList);cdecl;external gliblib name 'g_queue_push_tail_link';

  function  g_queue_pop_head_link(queue:PGQueue):PGList;cdecl;external gliblib name 'g_queue_pop_head_link';

  function  g_queue_pop_tail_link(queue:PGQueue):PGList;cdecl;external gliblib name 'g_queue_pop_tail_link';



