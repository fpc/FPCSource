// included by glib2.pas

{$IFDEF read_forward_definitions}
   PGList = ^TGList;
   TGList = record
        data : gpointer;
        next : PGList;
        prev : PGList;
     end;

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{ Doubly linked lists }

procedure g_list_push_allocator(allocator:PGAllocator);cdecl;external gliblib name 'g_list_push_allocator';

procedure g_list_pop_allocator;cdecl;external gliblib name 'g_list_pop_allocator';

function  g_list_alloc:PGList;cdecl;external gliblib name 'g_list_alloc';

procedure g_list_free(list:PGList);cdecl;external gliblib name 'g_list_free';

procedure g_list_free_1(list:PGList);cdecl;external gliblib name 'g_list_free_1';

function  g_list_append(list:PGList; data:gpointer):PGList;cdecl;external gliblib name 'g_list_append';

function  g_list_prepend(list:PGList; data:gpointer):PGList;cdecl;external gliblib name 'g_list_prepend';

function  g_list_insert(list:PGList; data:gpointer; position:gint):PGList;cdecl;external gliblib name 'g_list_insert';

function  g_list_insert_sorted(list:PGList; data:gpointer; func:TGCompareFunc):PGList;cdecl;external gliblib name 'g_list_insert_sorted';

function  g_list_insert_before(list:PGList; sibling:PGList; data:gpointer):PGList;cdecl;external gliblib name 'g_list_insert_before';

function  g_list_concat(list1:PGList; list2:PGList):PGList;cdecl;external gliblib name 'g_list_concat';

function  g_list_remove(list:PGList; data:gconstpointer):PGList;cdecl;external gliblib name 'g_list_remove';

function  g_list_remove_all(list:PGList; data:gconstpointer):PGList;cdecl;external gliblib name 'g_list_remove_all';

function  g_list_remove_link(list:PGList; llink:PGList):PGList;cdecl;external gliblib name 'g_list_remove_link';

function  g_list_delete_link(list:PGList; link:PGList):PGList;cdecl;external gliblib name 'g_list_delete_link';

function  g_list_reverse(list:PGList):PGList;cdecl;external gliblib name 'g_list_reverse';

function  g_list_copy(list:PGList):PGList;cdecl;external gliblib name 'g_list_copy';

function  g_list_nth(list:PGList; n:guint):PGList;cdecl;external gliblib name 'g_list_nth';

function  g_list_nth_prev(list:PGList; n:guint):PGList;cdecl;external gliblib name 'g_list_nth_prev';

function  g_list_find(list:PGList; data:gconstpointer):PGList;cdecl;external gliblib name 'g_list_find';

function  g_list_find_custom(list:PGList; data:gconstpointer; func:TGCompareFunc):PGList;cdecl;external gliblib name 'g_list_find_custom';

function  g_list_position(list:PGList; llink:PGList):gint;cdecl;external gliblib name 'g_list_position';

function  g_list_index(list:PGList; data:gconstpointer):gint;cdecl;external gliblib name 'g_list_index';

function  g_list_last(list:PGList):PGList;cdecl;external gliblib name 'g_list_last';

function  g_list_first(list:PGList):PGList;cdecl;external gliblib name 'g_list_first';

function  g_list_length(list:PGList):guint;cdecl;external gliblib name 'g_list_length';

procedure g_list_foreach(list:PGList; func:TGFunc; user_data:gpointer);cdecl;external gliblib name 'g_list_foreach';

function  g_list_sort(list:PGList; compare_func:TGCompareFunc):PGList;cdecl;external gliblib name 'g_list_sort';

function  g_list_sort_with_data(list:PGList; compare_func:TGCompareDataFunc; user_data:gpointer):PGList;cdecl;external gliblib name 'g_list_sort_with_data';

function  g_list_nth_data(list:PGList; n:guint):gpointer;cdecl;external gliblib name 'g_list_nth_data';

{ Macros }

function g_list_previous (list : PGList) : PGList;

function g_list_next  (list : PGList)    : PGList;
{$ENDIF read_interface_rest}

// included by glib2.pas

