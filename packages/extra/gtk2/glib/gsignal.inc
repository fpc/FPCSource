// included by glib2.pas

{ --- typedefs ---  }
type
   PGSignalInvocationHint = ^TGSignalInvocationHint;

   PGSignalCMarshaller = ^TGSignalCMarshaller;
   TGSignalCMarshaller = TGClosureMarshal;

   TGSignalEmissionHook = function (ihint:PGSignalInvocationHint; n_param_values:guint; param_values:PGValue; data:gpointer):gboolean; cdecl;

   TGSignalAccumulator = function (ihint:PGSignalInvocationHint; return_accu:PGValue; handler_return:PGValue; data:gpointer):gboolean; cdecl;

{ --- run, match and connect types ---  }
   PGSignalFlags = ^TGSignalFlags;
   TGSignalFlags = longint;

{ --- signal information ---  }
   TGSignalInvocationHint = record
        signal_id : guint;
        detail : TGQuark;
        run_type : TGSignalFlags;
     end;

{ mangled with G_SIGNAL_TYPE_STATIC_SCOPE flag  }
{ mangled with G_SIGNAL_TYPE_STATIC_SCOPE flag  }
   PGSignalQuery = ^TGSignalQuery;
   TGSignalQuery = record
        signal_id : guint;
        signal_name : Pgchar;
        itype : GType;
        signal_flags : TGSignalFlags;
        return_type : GType;
        n_params : guint;
        param_types : PGType;
     end;


const
   G_SIGNAL_RUN_FIRST = 1 shl 0;
   G_SIGNAL_RUN_LAST = 1 shl 1;
   G_SIGNAL_RUN_CLEANUP = 1 shl 2;
   G_SIGNAL_NO_RECURSE = 1 shl 3;
   G_SIGNAL_DETAILED = 1 shl 4;
   G_SIGNAL_ACTION = 1 shl 5;
   G_SIGNAL_NO_HOOKS = 1 shl 6;
   G_SIGNAL_FLAGS_MASK = $7f;

type
   PGConnectFlags = ^TGConnectFlags;
   TGConnectFlags = longint;

const
   G_CONNECT_AFTER = 1 shl 0;
   G_CONNECT_SWAPPED = 1 shl 1;

type
   PGSignalMatchType = ^TGSignalMatchType;
   TGSignalMatchType = longint;

const
   G_SIGNAL_MATCH_ID = 1 shl 0;
   G_SIGNAL_MATCH_DETAIL = 1 shl 1;
   G_SIGNAL_MATCH_CLOSURE = 1 shl 2;
   G_SIGNAL_MATCH_FUNC = 1 shl 3;
   G_SIGNAL_MATCH_DATA = 1 shl 4;
   G_SIGNAL_MATCH_UNBLOCKED = 1 shl 5;
   G_SIGNAL_MATCH_MASK = $3f;

   G_SIGNAL_TYPE_STATIC_SCOPE = G_TYPE_FLAG_RESERVED_ID_BIT;


{ --- signals ---  }

function g_signal_newv(signal_name:Pgchar; itype:GType; signal_flags:TGSignalFlags; class_closure:PGClosure; accumulator:TGSignalAccumulator;
           accu_data:gpointer; c_marshaller:TGSignalCMarshaller; return_type:GType; n_params:guint; param_types:PGType):guint; cdecl; external gobjectlib;
{$IFNDEF KYLIX}
function g_signal_new_valist(signal_name:Pgchar; itype:GType; signal_flags:TGSignalFlags; class_closure:PGClosure; accumulator:TGSignalAccumulator;
           accu_data:gpointer; c_marshaller:TGSignalCMarshaller; return_type:GType; n_params:guint; args: array of const):guint; cdecl; external gobjectlib;
function g_signal_new(signal_name:Pgchar; itype:GType; signal_flags:TGSignalFlags; class_offset:guint; accumulator:TGSignalAccumulator;
           accu_data:gpointer; c_marshaller:TGSignalCMarshaller; return_type:GType; n_params:guint; args:array of const):guint; cdecl; overload; external gobjectlib;
function g_signal_new(signal_name:Pgchar; itype:GType; signal_flags:TGSignalFlags; class_offset:guint; accumulator:TGSignalAccumulator;
           accu_data:gpointer; c_marshaller:TGSignalCMarshaller; return_type:GType; n_params:guint):guint; cdecl; overload; varargs; external gobjectlib;
{$ELSE}
function g_signal_new_valist(signal_name:Pgchar; itype:GType; signal_flags:TGSignalFlags; class_closure:PGClosure; accumulator:TGSignalAccumulator;
           accu_data:gpointer; c_marshaller:TGSignalCMarshaller; return_type:GType; n_params:guint):guint; varargs; cdecl; external gobjectlib;
function g_signal_new(signal_name:Pgchar; itype:GType; signal_flags:TGSignalFlags; class_offset:guint; accumulator:TGSignalAccumulator;
           accu_data:gpointer; c_marshaller:TGSignalCMarshaller; return_type:GType; n_params:guint):guint; varargs; cdecl; external gobjectlib;
{$ENDIF}
procedure g_signal_emitv(instance_and_params:PGValue; signal_id:guint; detail:TGQuark; return_value:PGValue); cdecl; external gobjectlib;
{$IFNDEF KYLIX}
procedure g_signal_emit_valist(instance:gpointer; signal_id:guint; detail:TGQuark; var_args: array of const); cdecl; external gobjectlib;
procedure g_signal_emit(instance:gpointer; signal_id:guint; detail:TGQuark; args:array of const); cdecl; overload; external gobjectlib;
procedure g_signal_emit(instance:gpointer; signal_id:guint; detail:TGQuark); cdecl; overload; varargs; external gobjectlib;
procedure g_signal_emit_by_name(instance:gpointer; detailed_signal:Pgchar; args:array of const); cdecl; overload; external gobjectlib;
procedure g_signal_emit_by_name(instance:gpointer; detailed_signal:Pgchar); cdecl; overload; varargs; external gobjectlib;
{$ELSE}
procedure g_signal_emit_valist(instance:gpointer; signal_id:guint; detail:TGQuark); varargs; cdecl; external gobjectlib;
procedure g_signal_emit(instance:gpointer; signal_id:guint; detail:TGQuark); varargs; cdecl; external gobjectlib;
procedure g_signal_emit_by_name(instance:gpointer; detailed_signal:Pgchar); varargs; cdecl; external gobjectlib;
{$ENDIF}
function g_signal_lookup(name:Pgchar; itype:GType):guint; cdecl; external gobjectlib;
function g_signal_name(signal_id:guint):Pgchar; cdecl; external gobjectlib;
procedure g_signal_query(signal_id:guint; query:PGSignalQuery); cdecl; external gobjectlib;
function g_signal_list_ids(itype:GType; n_ids:Pguint):Pguint; cdecl; external gobjectlib;
function g_signal_parse_name(detailed_signal:Pgchar; itype:GType; signal_id_p:Pguint; detail_p:PGQuark; force_detail_quark:gboolean):gboolean; cdecl; external gobjectlib;
function g_signal_get_invocation_hint(instance:gpointer):PGSignalInvocationHint; cdecl; external gobjectlib;
{ --- signal emissions ---  }
procedure g_signal_stop_emission(instance:gpointer; signal_id:guint; detail:TGQuark); cdecl; external gobjectlib;
procedure g_signal_stop_emission_by_name(instance:gpointer; detailed_signal:Pgchar); cdecl; external gobjectlib;
function g_signal_add_emission_hook(signal_id:guint; quark:TGQuark; hook_func:TGSignalEmissionHook; hook_data:gpointer; data_destroy:TGDestroyNotify):gulong; cdecl; external gobjectlib;
procedure g_signal_remove_emission_hook(signal_id:guint; hook_id:gulong); cdecl; external gobjectlib;
{ --- signal handlers ---  }
function g_signal_has_handler_pending(instance:gpointer; signal_id:guint; detail:TGQuark; may_be_blocked:gboolean):gboolean; cdecl; external gobjectlib;
function g_signal_connect_closure_by_id(instance:gpointer; signal_id:guint; detail:TGQuark; closure:PGClosure; after:gboolean):gulong; cdecl; external gobjectlib;
function g_signal_connect_closure(instance:gpointer; detailed_signal:Pgchar; closure:PGClosure; after:gboolean):gulong; cdecl; external gobjectlib;
function g_signal_connect_data(instance:gpointer; detailed_signal:Pgchar; c_handler:TGCallback; data:gpointer; destroy_data:TGClosureNotify;
           connect_flags:TGConnectFlags):gulong; cdecl; external gobjectlib;
procedure g_signal_handler_block(instance:gpointer; handler_id:gulong); cdecl; external gobjectlib;
procedure g_signal_handler_unblock(instance:gpointer; handler_id:gulong); cdecl; external gobjectlib;
procedure g_signal_handler_disconnect(instance:gpointer; handler_id:gulong); cdecl; external gobjectlib;
function g_signal_handler_is_connected(instance:gpointer; handler_id:gulong):gboolean; cdecl; external gobjectlib;
function g_signal_handler_find(instance: gpointer; mask: TGSignalMatchType;
           signal_id: guint; detail: TGQuark; closure: PGClosure;
           func:gpointer; data:gpointer):gulong; cdecl; external gobjectlib;
function g_signal_handlers_block_matched(instance:gpointer; mask:TGSignalMatchType; signal_id:guint; detail:TGQuark; closure:PGClosure;
           func:gpointer; data:gpointer):guint; cdecl; external gobjectlib;
function g_signal_handlers_unblock_matched(instance:gpointer; mask:TGSignalMatchType; signal_id:guint; detail:TGQuark; closure:PGClosure;
           func:gpointer; data:gpointer):guint; cdecl; external gobjectlib;
function g_signal_handlers_disconnect_matched(instance:gpointer; mask:TGSignalMatchType; signal_id:guint; detail:TGQuark; closure:PGClosure;
           func:gpointer; data:gpointer):guint; cdecl; external gobjectlib;
{ --- chaining for language bindings ---  }
procedure g_signal_override_class_closure(signal_id:guint; instance_type:GType; class_closure:PGClosure); cdecl; external gobjectlib;
procedure g_signal_chain_from_overridden(instance_and_params:PGValue; return_value:PGValue); cdecl; external gobjectlib;

{ --- convenience ---  }
function g_signal_connect(instance:gpointer; detailed_signal:Pgchar; c_handler:TGCallback; data:gpointer) : gulong;
function g_signal_connect_after(instance:gpointer; detailed_signal:Pgchar; c_handler:TGCallback; data:gpointer) : gulong;
function g_signal_connect_swapped(instance:gpointer; detailed_signal:Pgchar; c_handler:TGCallback; data:gpointer) : gulong;
function g_signal_handlers_disconnect_by_func(instance:gpointer; func, data: gpointer) : guint;

procedure g_signal_handlers_block_by_func(instance: gpointer; func, data: gpointer);
procedure g_signal_handlers_unblock_by_func(instance: gpointer; func, data: gpointer);

{< private > }
procedure g_signal_handlers_destroy(instance:gpointer); cdecl; external gobjectlib;
procedure _g_signals_destroy(itype:GType); cdecl; external gobjectlib;

// included by glib2.pas

