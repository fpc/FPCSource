// included by glib2.pas

{ --- typedefs & structures ---  }
type
{< private > }
   PGObject = ^TGObject;
   TGObject = record
        g_type_instance : TGTypeInstance;
        ref_count : guint;
        qdata : PGData;
     end;

   TGObjectGetPropertyFunc = procedure (anObject:PGObject; property_id:guint; value:PGValue; pspec:PGParamSpec); cdecl;

   TGObjectSetPropertyFunc = procedure (anObject:PGObject; property_id:guint; value:PGValue; pspec:PGParamSpec); cdecl;

   TGObjectFinalizeFunc = procedure (anObject:PGObject); cdecl;

   TGWeakNotify = procedure (data:gpointer; where_the_object_was:PGObject); cdecl;

{< private > }
   PGObjectConstructParam = ^TGObjectConstructParam;

{ public overridable methods  }
{ seldomly overidden  }
{ signals  }
{ padding  }
   PGObjectClass = ^TGObjectClass;
   TGObjectClass = record
        g_type_class : TGTypeClass;
        construct_properties : PGSList;
        _constructor : function (_type:GType; n_construct_properties:guint; construct_properties:PGObjectConstructParam):PGObject; cdecl;
        set_property : procedure (anObject:PGObject; property_id:guint; value:PGValue; pspec:PGParamSpec); cdecl;
        get_property : procedure (anObject:PGObject; property_id:guint; value:PGValue; pspec:PGParamSpec); cdecl;
        _dispose : procedure (anObject:PGObject); cdecl;
        finalize : procedure (anObject:PGObject); cdecl;
        dispatch_properties_changed : procedure (anObject:PGObject; n_pspecs:guint; pspecs:PPGParamSpec); cdecl;
        notify : procedure (anObject:PGObject; pspec:PGParamSpec); cdecl;
        pdummy : array[0..7] of gpointer;
     end;

   TGObjectConstructParam = record
        pspec : PGParamSpec;
        value : PGValue;
     end;

{ --- type macros ---  }

function G_TYPE_IS_OBJECT(_type : GType) : boolean;
function G_OBJECT(anObject: pointer) : PGObject;
function G_OBJECT_CLASS(_class : Pointer) : PGObjectClass;
function G_IS_OBJECT(anObject: pointer) : boolean;
function G_IS_OBJECT_CLASS(_class : Pointer) : boolean;
function G_OBJECT_GET_CLASS(anObject: pointer) : PGObjectClass;
function G_OBJECT_TYPE(anObject: pointer) : GType;
function G_OBJECT_TYPE_NAME(anObject: pointer) : Pgchar;
function G_OBJECT_CLASS_TYPE(_class : Pointer) : GType;
function G_OBJECT_CLASS_NAME(_class : Pointer) : Pgchar;
function G_VALUE_HOLDS_OBJECT(value : Pointer) : boolean;


{ --- prototypes ---  }

procedure g_object_class_install_property(oclass:PGObjectClass; property_id:guint; pspec:PGParamSpec); cdecl; external gobjectlib;
function g_object_class_find_property(oclass:PGObjectClass; property_name:Pgchar):PGParamSpec; cdecl; external gobjectlib;
function g_object_class_list_properties(oclass:PGObjectClass; n_properties:Pguint):PPGParamSpec; cdecl; external gobjectlib;
{$IFNDEF KYLIX}
function g_object_new(object_type:GType; first_property_name:Pgchar; args:array of const):gpointer; cdecl; overload; external gobjectlib;
function g_object_new(object_type:GType; first_property_name:Pgchar):gpointer; cdecl; overload; varargs; external gobjectlib;
function g_object_newv(object_type:GType; n_parameters:guint; parameters:PGParameter):gpointer; cdecl; external gobjectlib;
function g_object_new_valist(object_type:GType; first_property_name:Pgchar; var_args: array of const):PGObject; cdecl; external gobjectlib;
procedure g_object_set(anObject:gpointer; first_property_name:Pgchar; args:array of const); cdecl; overload; external gobjectlib;
procedure g_object_set(anObject:gpointer; first_property_name:Pgchar); cdecl; overload; varargs; external gobjectlib;
procedure g_object_get(anObject:gpointer; first_property_name:Pgchar; args:array of const); cdecl; overload; external gobjectlib;
procedure g_object_get(anObject:gpointer; first_property_name:Pgchar); cdecl; overload; varargs; external gobjectlib;
function g_object_connect(anObject:gpointer; signal_spec:Pgchar; args:array of const):gpointer; cdecl; overload; external gobjectlib;
function g_object_connect(anObject:gpointer; signal_spec:Pgchar):gpointer; cdecl; overload; varargs; external gobjectlib;
procedure g_object_disconnect(anObject:gpointer; signal_spec:Pgchar; args:array of const); cdecl; overload; external gobjectlib;
procedure g_object_disconnect(anObject:gpointer; signal_spec:Pgchar); cdecl; overload; varargs; external gobjectlib;
procedure g_object_set_valist(anObject:PGObject; first_property_name:Pgchar; var_args: array of const); cdecl; external gobjectlib;
procedure g_object_get_valist(anObject:PGObject; first_property_name:Pgchar; var_args: array of const); cdecl; external gobjectlib;
{$ELSE}
function g_object_new(object_type:GType; first_property_name:Pgchar):gpointer; varargs; cdecl; external gobjectlib;
function g_object_newv(object_type:GType; n_parameters:guint; parameters:PGParameter):gpointer; varargs; cdecl; external gobjectlib;
function g_object_new_valist(object_type:GType; first_property_name:Pgchar):PGObject; varargs; cdecl; external gobjectlib;
procedure g_object_set(anObject:gpointer; first_property_name:Pgchar); varargs; cdecl; external gobjectlib;
procedure g_object_get(anObject:gpointer; first_property_name:Pgchar); varargs; cdecl; external gobjectlib;
function g_object_connect(anObject:gpointer; signal_spec:Pgchar):gpointer; varargs; cdecl; external gobjectlib;
procedure g_object_disconnect(anObject:gpointer; signal_spec:Pgchar); varargs; cdecl; external gobjectlib;
procedure g_object_set_valist(anObject:PGObject; first_property_name:Pgchar); varargs; cdecl; external gobjectlib;
procedure g_object_get_valist(anObject:PGObject; first_property_name:Pgchar); varargs; cdecl; external gobjectlib;
{$ENDIF}
procedure g_object_set_property(anObject:PGObject; property_name:Pgchar; value:PGValue); cdecl; external gobjectlib;
procedure g_object_get_property(anObject:PGObject; property_name:Pgchar; value:PGValue); cdecl; external gobjectlib;
procedure g_object_freeze_notify(anObject:PGObject); cdecl; external gobjectlib;
procedure g_object_notify(anObject:PGObject; property_name:Pgchar); cdecl; external gobjectlib;
procedure g_object_thaw_notify(anObject:PGObject); cdecl; external gobjectlib;
function g_object_ref(anObject:gpointer):gpointer; cdecl; external gobjectlib;
procedure g_object_unref(anObject:gpointer); cdecl; external gobjectlib;
procedure g_object_weak_ref(anObject:PGObject; notify:TGWeakNotify; data:gpointer); cdecl; external gobjectlib;
procedure g_object_weak_unref(anObject:PGObject; notify:TGWeakNotify; data:gpointer); cdecl; external gobjectlib;
procedure g_object_add_weak_pointer(anObject:PGObject; weak_pointer_location:Pgpointer); cdecl; external gobjectlib;
procedure g_object_remove_weak_pointer(anObject:PGObject; weak_pointer_location:Pgpointer); cdecl; external gobjectlib;
function g_object_get_qdata(anObject:PGObject; quark:TGQuark):gpointer; cdecl; external gobjectlib;
procedure g_object_set_qdata(anObject:PGObject; quark:TGQuark; data:gpointer); cdecl; external gobjectlib;
procedure g_object_set_qdata_full(anObject:PGObject; quark:TGQuark; data:gpointer; destroy:TGDestroyNotify); cdecl; external gobjectlib;
function g_object_steal_qdata(anObject:PGObject; quark:TGQuark):gpointer; cdecl; external gobjectlib;
function g_object_get_data(anObject:PGObject; key:Pgchar):gpointer; cdecl; external gobjectlib;
procedure g_object_set_data(anObject:PGObject; key:Pgchar; data:gpointer); cdecl; external gobjectlib;
procedure g_object_set_data_full(anObject:PGObject; key:Pgchar; data:gpointer; destroy:TGDestroyNotify); cdecl; external gobjectlib;
function g_object_steal_data(anObject:PGObject; key:Pgchar):gpointer; cdecl; external gobjectlib;
procedure g_object_watch_closure(anObject:PGObject; closure:PGClosure); cdecl; external gobjectlib;
function g_cclosure_new_object(callback_func:TGCallback; anObject:PGObject):PGClosure; cdecl; external gobjectlib;
function g_cclosure_new_object_swap(callback_func:TGCallback; anObject:PGObject):PGClosure; cdecl; external gobjectlib;
function g_closure_new_object(sizeof_closure:guint; anObject:PGObject):PGClosure; cdecl; external gobjectlib;
procedure g_value_set_object(value:PGValue; v_object:gpointer); cdecl; external gobjectlib;
function g_value_get_object(value:PGValue):gpointer; cdecl; external gobjectlib;
function g_value_dup_object(value:PGValue):PGObject; cdecl; external gobjectlib;
function g_signal_connect_object(instance:gpointer; detailed_signal:Pgchar; c_handler:TGCallback; gobject:gpointer; connect_flags:TGConnectFlags):gulong; cdecl; external gobjectlib;
{< protected > }
procedure g_object_run_dispose(anObject:PGObject); cdecl; external gobjectlib;
{ --- marshaller specific ---  }
procedure g_value_set_object_take_ownership(value:PGValue; v_object:gpointer); cdecl; external gobjectlib;


{ --- implementation macros ---  }
procedure G_OBJECT_WARN_INVALID_PSPEC(anObject: gpointer; pname: PGChar;
  property_id: gint; pspec: gpointer);
procedure G_OBJECT_WARN_INVALID_PROPERTY_ID(anObject: gpointer;
  property_id: gint; pspec : gpointer);

type
  G_FLAGS_TYPE = GType;

// included by glib2.pas

