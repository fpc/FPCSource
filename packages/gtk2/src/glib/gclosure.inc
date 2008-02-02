// included by glib2.pas

{ -- typedefs ---  }
type
   PGClosure = ^TGClosure;
   PGClosureNotifyData = ^TGClosureNotifyData;
   TGClosureNotify = procedure (data:gpointer; closure:PGClosure); cdecl;

{< private > }
{< private > }
{< private > }
{< private > }
{ finalization notifiers  }
{< private > }
{ invalidation notifiers  }
{< private > }
{< private > }
{< protected > }
{< public > }
{< public > }
{< private > }
{out }
{< protected > }
{< private > }
{ invariants/constrains:
     - ->marshal and ->data are _invalid_ as soon as ->is_invalid==TRUE
     - invocation of all inotifiers occours prior to fnotifiers
     - order of inotifiers is random
       inotifiers may _not_ free/invalidate parameter values (e.g. ->data)
     - order of fnotifiers is random
     - each notifier may only be removed before or during its invocation
     - reference counting may only happen prior to fnotify invocation
       (in that sense, fnotifiers are really finalization handlers)
    }
   TGClosure = record
        flag0 : longint;
        marshal : procedure (closure:PGClosure; return_value:PGValue; n_param_values:guint; param_values:PGValue; invocation_hint:gpointer;
                      marshal_data:gpointer); cdecl;
        data : gpointer;
        notifiers : PGClosureNotifyData;
     end;


   TGCallBackProcedure = procedure;
   TGCallback = procedure (para1: TGCallBackProcedure); cdecl;

   TGClosureMarshal = procedure (closure:PGClosure; return_value:PGValue; n_param_values:guint; param_values:PGValue; invocation_hint:gpointer;
                 marshal_data:gpointer); cdecl;
{ --- structures ---  }
   TGClosureNotifyData = record
        data : gpointer;
        notify : TGClosureNotify;
     end;


{ --- defines ---  }

function G_CLOSURE_NEEDS_MARSHAL(closure : Pointer) : boolean;
function G_CLOSURE_N_NOTIFIERS(cl : PGClosure) : longint;
function G_CCLOSURE_SWAP_DATA(cclosure : PGClosure) : longint;
function G_CALLBACK(f : pointer) : TGCallback;

const
   bm_TGClosure_ref_count = $7FFF;
   bp_TGClosure_ref_count = 0;
   bm_TGClosure_meta_marshal = $8000;
   bp_TGClosure_meta_marshal = 15;
   bm_TGClosure_n_guards = $10000;
   bp_TGClosure_n_guards = 16;
   bm_TGClosure_n_fnotifiers = $60000;
   bp_TGClosure_n_fnotifiers = 17;
   bm_TGClosure_n_inotifiers = $7F80000;
   bp_TGClosure_n_inotifiers = 19;
   bm_TGClosure_in_inotify = $8000000;
   bp_TGClosure_in_inotify = 27;
   bm_TGClosure_floating = $10000000;
   bp_TGClosure_floating = 28;
   bm_TGClosure_derivative_flag = $20000000;
   bp_TGClosure_derivative_flag = 29;
   bm_TGClosure_in_marshal = $40000000;
   bp_TGClosure_in_marshal = 30;
   bm_TGClosure_is_invalid = $80000000;
   bp_TGClosure_is_invalid = 31;

function ref_count(var a : TGClosure) : guint;
procedure set_ref_count(var a : TGClosure; __ref_count : guint);
function meta_marshal(a : PGClosure) : guint;
procedure set_meta_marshal(var a : TGClosure; __meta_marshal : guint);
function n_guards(a : PGClosure) : guint;
procedure set_n_guards(var a : TGClosure; __n_guards : guint);
function n_fnotifiers(a : PGClosure) : guint;
procedure set_n_fnotifiers(var a : TGClosure; __n_fnotifiers : guint);
function n_inotifiers(a : PGClosure) : guint;
procedure set_n_inotifiers(var a : TGClosure; __n_inotifiers : guint);
function in_inotify(var a : TGClosure) : guint;
procedure set_in_inotify(var a : TGClosure; __in_inotify : guint);
function floating(var a : TGClosure) : guint;
procedure set_floating(var a : TGClosure; __floating : guint);
function derivative_flag(a : PGClosure) : guint;
procedure set_derivative_flag(var a : TGClosure; __derivative_flag : guint);
function in_marshal(var a : TGClosure) : guint;
procedure set_in_marshal(var a : TGClosure; __in_marshal : guint);
function is_invalid(var a : TGClosure) : guint;
procedure set_is_invalid(var a : TGClosure; __is_invalid : guint);


{ closure for C function calls, callback() is the user function }
type
   PGCClosure = ^TGCClosure;
   TGCClosure = record
        closure : TGClosure;
        callback : gpointer;
     end;

{ --- prototypes ---  }

function g_cclosure_new(callback_func:TGCallback; user_data:gpointer; destroy_data:TGClosureNotify):PGClosure; cdecl; external gliblib;
function g_cclosure_new_swap(callback_func:TGCallback; user_data:gpointer; destroy_data:TGClosureNotify):PGClosure; cdecl; external gliblib;
function g_signal_type_cclosure_new(itype:GType; struct_offset:guint):PGClosure; cdecl; external gliblib;
{ --- prototypes ---  }
function g_closure_ref(closure:PGClosure):PGClosure; cdecl; external gliblib;
procedure g_closure_sink(closure:PGClosure); cdecl; external gliblib;
procedure g_closure_unref(closure:PGClosure); cdecl; external gliblib;
{ intimidating  }
function g_closure_new_simple(sizeof_closure:guint; data:gpointer):PGClosure; cdecl; external gliblib;
procedure g_closure_add_finalize_notifier(closure:PGClosure; notify_data:gpointer; notify_func:TGClosureNotify); cdecl; external gliblib;
procedure g_closure_remove_finalize_notifier(closure:PGClosure; notify_data:gpointer; notify_func:TGClosureNotify); cdecl; external gliblib;
procedure g_closure_add_invalidate_notifier(closure:PGClosure; notify_data:gpointer; notify_func:TGClosureNotify); cdecl; external gliblib;
procedure g_closure_remove_invalidate_notifier(closure:PGClosure; notify_data:gpointer; notify_func:TGClosureNotify); cdecl; external gliblib;
procedure g_closure_add_marshal_guards(closure:PGClosure; pre_marshal_data:gpointer; pre_marshal_notify:TGClosureNotify; post_marshal_data:gpointer; post_marshal_notify:TGClosureNotify); cdecl; external gliblib;
procedure g_closure_set_marshal(closure:PGClosure; marshal:TGClosureMarshal); cdecl; external gliblib;
procedure g_closure_set_meta_marshal(closure:PGClosure; marshal_data:gpointer; meta_marshal:TGClosureMarshal); cdecl; external gliblib;
procedure g_closure_invalidate(closure:PGClosure); cdecl; external gliblib;
{out }
procedure g_closure_invoke(closure:PGClosure; return_value:PGValue; n_param_values:guint; param_values:PGValue; invocation_hint:gpointer); cdecl; external gliblib;

{ FIXME:
   OK:  data_object::destroy            -> closure_invalidate();
   MIS: closure_invalidate()            -> disconnect(closure);
   MIS: disconnect(closure)             -> (unlink) closure_unref();
   OK:  closure_finalize()              -> g_free (data_string);

   random remarks:
   - need marshaller repo with decent aliasing to base types
   - provide marshaller collection, virtually covering anything out there
 }

// included by glib2.pas

