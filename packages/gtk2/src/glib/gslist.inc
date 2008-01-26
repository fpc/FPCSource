// included by glib2.pas

{$IFDEF read_forward_definitions}
   PGSList = ^TGSList;
   PPGSList = ^PGSList;
   TGSList = record
          data : gpointer;
          next : PGSList;
       end;

{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
{ Singly linked lists
  }

procedure g_slist_push_allocator(allocator:PGAllocator);cdecl;external gliblib name 'g_slist_push_allocator';

procedure g_slist_pop_allocator;cdecl;external gliblib name 'g_slist_pop_allocator';

function g_slist_alloc:PGSList;cdecl;external gliblib name 'g_slist_alloc';

procedure g_slist_free(list:PGSList);cdecl;external gliblib name 'g_slist_free';

procedure g_slist_free_1(list:PGSList);cdecl;external gliblib name 'g_slist_free_1';

function g_slist_append(list:PGSList; data:gpointer):PGSList;cdecl;external gliblib name 'g_slist_append';

function g_slist_prepend(list:PGSList; data:gpointer):PGSList;cdecl;external gliblib name 'g_slist_prepend';

function g_slist_insert(list:PGSList; data:gpointer; position:gint):PGSList;cdecl;external gliblib name 'g_slist_insert';

function g_slist_insert_sorted(list:PGSList; data:gpointer; func:TGCompareFunc):PGSList;cdecl;external gliblib name 'g_slist_insert_sorted';

function g_slist_insert_before(slist:PGSList; sibling:PGSList; data:gpointer):PGSList;cdecl;external gliblib name 'g_slist_insert_before';

function g_slist_concat(list1:PGSList; list2:PGSList):PGSList;cdecl;external gliblib name 'g_slist_concat';

function g_slist_remove(list:PGSList; data:gconstpointer):PGSList;cdecl;external gliblib name 'g_slist_remove';

function g_slist_remove_all(list:PGSList; data:gconstpointer):PGSList;cdecl;external gliblib name 'g_slist_remove_all';

function g_slist_remove_link(list:PGSList; link:PGSList):PGSList;cdecl;external gliblib name 'g_slist_remove_link';

function g_slist_delete_link(list:PGSList; link:PGSList):PGSList;cdecl;external gliblib name 'g_slist_delete_link';

function g_slist_reverse(list:PGSList):PGSList;cdecl;external gliblib name 'g_slist_reverse';

function g_slist_copy(list:PGSList):PGSList;cdecl;external gliblib name 'g_slist_copy';

function g_slist_nth(list:PGSList; n:guint):PGSList;cdecl;external gliblib name 'g_slist_nth';

function g_slist_find(list:PGSList; data:gconstpointer):PGSList;cdecl;external gliblib name 'g_slist_find';

function g_slist_find_custom(list:PGSList; data:gconstpointer; func:TGCompareFunc):PGSList;cdecl;external gliblib name 'g_slist_find_custom';

function g_slist_position(list:PGSList; llink:PGSList):gint;cdecl;external gliblib name 'g_slist_position';

function g_slist_index(list:PGSList; data:gconstpointer):gint;cdecl;external gliblib name 'g_slist_index';

function g_slist_last(list:PGSList):PGSList;cdecl;external gliblib name 'g_slist_last';

function g_slist_length(list:PGSList):guint;cdecl;external gliblib name 'g_slist_length';

procedure g_slist_foreach(list:PGSList; func:TGFunc; user_data:gpointer);cdecl;external gliblib name 'g_slist_foreach';

function g_slist_sort(list:PGSList; compare_func:TGCompareFunc):PGSList;cdecl;external gliblib name 'g_slist_sort';

function g_slist_sort_with_data(list:PGSList; compare_func:TGCompareDataFunc; user_data:gpointer):PGSList;cdecl;external gliblib name 'g_slist_sort_with_data';

function g_slist_nth_data(list:PGSList; n:guint):gpointer;cdecl;external gliblib name 'g_slist_nth_data';

function g_slist_next(slist : PGSList) : PGSList;

{$ENDIF read_interface_rest}

// included by glib2.pas

