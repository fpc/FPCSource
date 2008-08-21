// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGHook = ^TGHook;
  TGHook = record
    data : gpointer;
    next : PGHook;
    prev : PGHook;
    ref_count : guint;
    hook_id   : gulong;
    flags   : guint;
    func    : gpointer;
    destroy : TGDestroyNotify;
  end;

  PGHookList = ^TGHookList;

  TGHookCompareFunc     = function  (new_hook:PGHook; sibling:PGHook):gint;cdecl;
  TGHookFindFunc        = function  (hook:PGHook;     data:gpointer):gboolean;cdecl;
  TGHookMarshaller      = procedure (hook:PGHook; marshal_data:gpointer);cdecl;
  TGHookCheckMarshaller = function (hook:PGHook; marshal_data:gpointer):gboolean;cdecl;
  TGHookFunc            = procedure (data:gpointer);cdecl;
  TGHookCheckFunc       = function (data:gpointer):gboolean;cdecl;
  TGHookFinalizeFunc    = procedure (hook_list:PGHookList; hook:PGHook);cdecl;

  TGHookList = record
    seq_id : gulong;
    flag0  : longint;
    hooks  : PGHook;
    hook_memchunk : PGMemChunk;
    finalize_hook : TGHookFinalizeFunc;
    dummy : array[0..1] of gpointer;
  end;

  PGHookFlagMask = ^TGHookFlagMask;
  TGHookFlagMask = integer;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

const
  G_HOOK_FLAG_ACTIVE = 1 shl 0;
  G_HOOK_FLAG_IN_CALL = 1 shl 1;
  G_HOOK_FLAG_MASK = $0f;

const
  G_HOOK_FLAG_USER_SHIFT = 4;

   { added for manipulating the TGHookList structure }

  bm_TGHookList_hook_size = $FFFF;
  bp_TGHookList_hook_size = 0;
  bm_TGHookList_is_setup  = $10000;
  bp_TGHookList_is_setup  = 16;

function  TGHookList_hook_size     (var a : TGHookList) : guint;
procedure TGHookList_set_hook_size (var a : TGHookList; __hook_size : guint);
function  TGHookList_is_setup      (var a : TGHookList) : guint;
procedure TGHookList_set_is_setup  (var a : TGHookList; __is_setup : guint);

{ --- macros ---  }

function G_HOOK(hook : pointer) : PGHook;
function G_HOOK_FLAGS(hook : PGHook) : guint;
function G_HOOK_ACTIVE (hook: PGHook): boolean;
function G_HOOK_IN_CALL (hook: PGHook): boolean;
function G_HOOK_IS_VALID (hook: PGHook): boolean;
function G_HOOK_IS_UNLINKED (hook: PGHook): boolean;

{ --- prototypes ---  }

procedure g_hook_list_init  (hook_list : PGHookList; hook_size : guint);cdecl; external gliblib name 'g_hook_list_init';
procedure g_hook_list_clear(hook_list:PGHookList);cdecl;external gliblib name 'g_hook_list_clear';
function g_hook_alloc(hook_list:PGHookList):PGHook;cdecl;external gliblib name 'g_hook_alloc';
procedure g_hook_free(hook_list:PGHookList; hook:PGHook);cdecl;external gliblib name 'g_hook_free';
procedure g_hook_ref(hook_list:PGHookList; hook:PGHook);cdecl;external gliblib name 'g_hook_ref';
procedure g_hook_unref(hook_list:PGHookList; hook:PGHook);cdecl;external gliblib name 'g_hook_unref';
function g_hook_destroy(hook_list:PGHookList; hook_id:gulong):gboolean;cdecl;external gliblib name 'g_hook_destroy';
procedure g_hook_destroy_link(hook_list:PGHookList; hook:PGHook);cdecl;external gliblib name 'g_hook_destroy_link';
procedure g_hook_prepend(hook_list:PGHookList; hook:PGHook);cdecl;external gliblib name 'g_hook_prepend';
procedure g_hook_insert_before(hook_list:PGHookList; sibling:PGHook; hook:PGHook);cdecl;external gliblib name 'g_hook_insert_before';
procedure g_hook_insert_sorted(hook_list:PGHookList; hook:PGHook; func:TGHookCompareFunc);cdecl;external gliblib name 'g_hook_insert_sorted';
function g_hook_get(hook_list:PGHookList; hook_id:gulong):PGHook;cdecl;external gliblib name 'g_hook_get';
function g_hook_find(hook_list:PGHookList; need_valids:gboolean; func:TGHookFindFunc; data:gpointer):PGHook;cdecl;external gliblib name 'g_hook_find';
function g_hook_find_data(hook_list:PGHookList; need_valids:gboolean; data:gpointer):PGHook;cdecl;external gliblib name 'g_hook_find_data';
function g_hook_find_func(hook_list:PGHookList; need_valids:gboolean; func:gpointer):PGHook;cdecl;external gliblib name 'g_hook_find_func';
function g_hook_find_func_data(hook_list:PGHookList; need_valids:gboolean; func:gpointer; data:gpointer):PGHook;cdecl;external gliblib name 'g_hook_find_func_data';
function g_hook_first_valid(hook_list:PGHookList; may_be_in_call:gboolean):PGHook;cdecl;external gliblib name 'g_hook_first_valid';
function g_hook_next_valid(hook_list:PGHookList; hook:PGHook; may_be_in_call:gboolean):PGHook;cdecl;external gliblib name 'g_hook_next_valid';
function g_hook_compare_ids(new_hook:PGHook; sibling:PGHook):gint;cdecl;external gliblib name 'g_hook_compare_ids';

{ convenience macros  }

procedure g_hook_append (hook_list: PGHookList; hook:PGHook);

procedure g_hook_list_invoke_check(hook_list:PGHookList; may_recurse:gboolean);cdecl;external gliblib name 'g_hook_list_invoke_check';
procedure g_hook_list_marshal(hook_list:PGHookList; may_recurse:gboolean; marshaller:TGHookMarshaller; marshal_data:gpointer);cdecl;external gliblib name 'g_hook_list_marshal';
procedure g_hook_list_marshal_check(hook_list:PGHookList; may_recurse:gboolean; marshaller:TGHookCheckMarshaller; marshal_data:gpointer);cdecl;external gliblib name 'g_hook_list_marshal_check';

{$ENDIF read_interface_rest}
// included by glib2.pas






