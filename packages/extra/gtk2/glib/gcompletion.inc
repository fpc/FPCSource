// {$include <glib/glist.h>}

    type
       PGCompletionFunc = ^TGCompletionFunc;
       TGCompletionFunc = gchar;

{ GCompletion   }

       TGCompletionStrncmpFunc = function (s1:Pgchar; s2:Pgchar; n:gsize):gint;cdecl;
       PGCompletion = ^TGCompletion;
       TGCompletion = record
            items  : PGList;
            func   : TGCompletionFunc;
            prefix : Pgchar;
            cache  : PGList;
            strncmp_func : TGCompletionStrncmpFunc;
         end;


    function  g_completion_new(func:TGCompletionFunc):PGCompletion;cdecl;external gliblib name 'g_completion_new';

    procedure g_completion_add_items(cmp:PGCompletion; items:PGList);cdecl;external gliblib name 'g_completion_add_items';

    procedure g_completion_remove_items(cmp:PGCompletion; items:PGList);cdecl;external gliblib name 'g_completion_remove_items';

    procedure g_completion_clear_items(cmp:PGCompletion);cdecl;external gliblib name 'g_completion_clear_items';

    function  g_completion_complete(cmp:PGCompletion; prefix:Pgchar; new_prefix:PPgchar):PGList;cdecl;external gliblib name 'g_completion_complete';

    procedure g_completion_set_compare(cmp:PGCompletion; strncmp_func:TGCompletionStrncmpFunc);cdecl;external gliblib name 'g_completion_set_compare';

    procedure g_completion_free(cmp:PGCompletion);cdecl;external gliblib name 'g_completion_free';

