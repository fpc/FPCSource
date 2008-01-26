
{$ifndef __G_ARRAY_H__}
{$define __G_ARRAY_H__}


//{$include gtypes.inc}

    type
       PGArray = ^TGArray;
       TGArray = record
            data : Pgchar;
            len : guint;
         end;

       PGByteArray = ^TGByteArray;
       TGByteArray = record
            data : Pguint8;
            len : guint;
         end;

       PGPtrArray = ^TGPtrArray;
       TGPtrArray = record
            pdata : Pgpointer;
            len : guint;
         end;




{* Resizable arrays, remove fills any cleared spot and shortens the
 * array, while preserving the order. remove_fast will distort the
 * order by moving the last element to the position of the removed
 *}


{ macro function;  see glib.cc for implementation}

    function g_array_append_val(a: PGArray; v : gpointer) : PGArray;

    function g_array_prepend_val(a: PGArray; v : gpointer) : PGArray;

    function g_array_insert_val(a: PGArray; i: guint; v : gpointer) : PGArray;



    function g_array_new(zero_terminated:gboolean; clear:gboolean; element_size:guint):PGArray;cdecl;external gliblib name 'g_array_new';

    function g_array_sized_new(zero_terminated:gboolean; clear:gboolean; element_size:guint; reserved_size:guint):PGArray;cdecl;external gliblib name 'g_array_sized_new';

    function g_array_free(parray:PGArray; free_segment:gboolean):Pgchar;cdecl;external gliblib name 'g_array_free';

    function g_array_append_vals(parray:PGArray; data:gconstpointer; len:guint):PGArray;cdecl;external gliblib name 'g_array_append_vals';

    function g_array_prepend_vals(parray:PGArray; data:gconstpointer; len:guint):PGArray;cdecl;external gliblib name 'g_array_prepend_vals';

    function g_array_insert_vals(parray:PGArray; index:guint; data:gconstpointer; len:guint):PGArray;cdecl;external gliblib name 'g_array_insert_vals';

    function g_array_set_size(parray:PGArray; length:guint):PGArray;cdecl;external gliblib name 'g_array_set_size';

    function g_array_remove_index(parray:PGArray; index:guint):PGArray;cdecl;external gliblib name 'g_array_remove_index';

    function g_array_remove_index_fast(parray:PGArray; index:guint):PGArray;cdecl;external gliblib name 'g_array_remove_index_fast';

    procedure g_array_sort(parray:PGArray; compare_func:TGCompareFunc);cdecl;external gliblib name 'g_array_sort';

    procedure g_array_sort_with_data(parray:PGArray; compare_func:TGCompareDataFunc; user_data:gpointer);cdecl;external gliblib name 'g_array_sort_with_data';

{* Resizable pointer array.  This interface is much less complicated
 * than the above.  Add appends appends a pointer.  Remove fills any
 * cleared spot and shortens the array. remove_fast will again distort
 * order.
 *}

 { macro function }
    function g_ptr_array_index (parray: PGPtrArray; index: guint): gpointer;


    function g_ptr_array_new:PGPtrArray;cdecl;external gliblib name 'g_ptr_array_new';

    function g_ptr_array_sized_new(reserved_size:guint):PGPtrArray;cdecl;external gliblib name 'g_ptr_array_sized_new';

    function g_ptr_array_free(parray:PGPtrArray; free_seg:gboolean):Pgpointer;cdecl;external gliblib name 'g_ptr_array_free';

    procedure g_ptr_array_set_size(parray:PGPtrArray; length:gint);cdecl;external gliblib name 'g_ptr_array_set_size';

    function g_ptr_array_remove_index(parray:PGPtrArray; index:guint):gpointer;cdecl;external gliblib name 'g_ptr_array_remove_index';

    function g_ptr_array_remove_index_fast(parray:PGPtrArray; index:guint):gpointer;cdecl;external gliblib name 'g_ptr_array_remove_index_fast';

    function g_ptr_array_remove(parray:PGPtrArray; data:gpointer):gboolean;cdecl;external gliblib name 'g_ptr_array_remove';

    function g_ptr_array_remove_fast(parray:PGPtrArray; data:gpointer):gboolean;cdecl;external gliblib name 'g_ptr_array_remove_fast';

    procedure g_ptr_array_add(parray:PGPtrArray; data:gpointer);cdecl;external gliblib name 'g_ptr_array_add';

    procedure g_ptr_array_sort(parray:PGPtrArray; compare_func:TGCompareFunc);cdecl;external gliblib name 'g_ptr_array_sort';

    procedure g_ptr_array_sort_with_data(parray:PGPtrArray; compare_func:TGCompareDataFunc; user_data:gpointer);cdecl;external gliblib name 'g_ptr_array_sort_with_data';



    function g_byte_array_new:PGByteArray;cdecl;external gliblib name 'g_byte_array_new';

    function g_byte_array_sized_new(reserved_size:guint):PGByteArray;cdecl;external gliblib name 'g_byte_array_sized_new';

    function g_byte_array_free(parray:PGByteArray; free_segment:gboolean):Pguint8;cdecl;external gliblib name 'g_byte_array_free';

    function g_byte_array_append(parray:PGByteArray; data:Pguint8; len:guint):PGByteArray;cdecl;external gliblib name 'g_byte_array_append';

    function g_byte_array_prepend(parray:PGByteArray; data:Pguint8; len:guint):PGByteArray;cdecl;external gliblib name 'g_byte_array_prepend';

    function g_byte_array_set_size(parray:PGByteArray; length:guint):PGByteArray;cdecl;external gliblib name 'g_byte_array_set_size';

    function g_byte_array_remove_index(parray:PGByteArray; index:guint):PGByteArray;cdecl;external gliblib name 'g_byte_array_remove_index';

    function g_byte_array_remove_index_fast(parray:PGByteArray; index:guint):PGByteArray;cdecl;external gliblib name 'g_byte_array_remove_index_fast';

    procedure g_byte_array_sort(parray:PGByteArray; compare_func:TGCompareFunc);cdecl;external gliblib name 'g_byte_array_sort';

    procedure g_byte_array_sort_with_data(parray:PGByteArray; compare_func:TGCompareDataFunc; user_data:gpointer);cdecl;external gliblib name 'g_byte_array_sort_with_data';

{$endif}




