// included by gtk2.pas

{$IFDEF read_forward_definitions}
   PGtkObject = ^TGtkObject;
   PPGtkObject = ^PGtkObject;
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ GtkObject only uses the first 4 bits of the flags field.
   Derived objects may use the remaining bits. Though this
   is a kinda nasty break up, it does make the size of
   derived objects smaller.
  }
{ Used internally during dispose  }

   PGtkObjectFlags = ^TGtkObjectFlags;
   TGtkObjectFlags = longint;


{ The GtkObject structure is the base of the Gtk+ objects hierarchy,
   it ``inherits'' from the GtkTypeObject by mirroring its fields,
   which must always be kept in sync completely. The GtkObject defines
   the few basic items that all derived classes contain.
  }
{ 32 bits of flags. GtkObject only uses 4 of these bits and
      GtkWidget uses the rest. This is done because structs are
      aligned on 4 or 8 byte boundaries. If a new bitfield were
      used in GtkWidget much space would be wasted.
    }
   TGtkObject = record
        parent_instance : TGObject;
        flags : guint32;
     end;

{ The GtkObjectClass is the base of the Gtk+ objects classes hierarchy,
   it ``inherits'' from the GtkTypeClass by mirroring its fields, which
   must always be kept in sync completely. The GtkObjectClass defines
   the basic necessities for the anObject inheritance mechanism to work.
  }
{ Non overridable class methods to set and get per class arguments  }
{ The function that will theEnd an objects life time. In one way ore
      another all three of them are defined for all objects. If an
      anObject class overrides one of the methods in order to perform class
      specific destruction then it must still invoke its superclass'
      implementation of the method after it is finished with its
      own cleanup. (See the destroy function for GtkWidget for
      an example of how to do this).
    }
   PGtkObjectClass = ^TGtkObjectClass;
   TGtkObjectClass = record
        parent_class : TGObjectClass;
        set_arg : procedure (anObject:PGtkObject; arg:PGtkArg; arg_id:guint); cdecl;
        get_arg : procedure (anObject:PGtkObject; arg:PGtkArg; arg_id:guint); cdecl;
        destroy : procedure (anObject:PGtkObject); cdecl;
     end;

{$ifndef GTK_DISABLE_DEPRECATED }
{ GtkArg flag bits for gtk_object_add_arg_type
  }
   PGtkArgFlags = ^TGtkArgFlags;
   TGtkArgFlags = longint;
{$endif}
{ GTK_DISABLE_DEPRECATED  }

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_IN_DESTRUCTION = 1 shl 0;
   GTK_FLOATING       = 1 shl 1;
   GTK_RESERVED_1     = 1 shl 2;
   GTK_RESERVED_2     = 1 shl 3;

   GTK_ARG_READABLE   = G_PARAM_READABLE;
   GTK_ARG_WRITABLE   = G_PARAM_WRITABLE;
   GTK_ARG_CONSTRUCT  = G_PARAM_CONSTRUCT;
   GTK_ARG_CONSTRUCT_ONLY = G_PARAM_CONSTRUCT_ONLY;
   GTK_ARG_CHILD_ARG  = 1 shl 4;

{ macros for casting a pointer to a GtkObject or GtkObjectClass pointer,
   and to test whether `anObject' and `klass' are of type GTK_TYPE_OBJECT.
   these are the standard macros for all GtkObject-derived classes.
  }
function GTK_TYPE_OBJECT : GType;
function GTK_OBJECT(anObject: pointer) : PGtkObject;
function GTK_OBJECT_CLASS(klass: pointer) : PGtkObjectClass;
function GTK_IS_OBJECT(anObject: pointer) : boolean;
function GTK_IS_OBJECT_CLASS(klass: pointer) : boolean;
function GTK_OBJECT_GET_CLASS(anObject: pointer) : PGtkObjectClass;

{ Macros for extracting various fields from GtkObject and GtkObjectClass.
  }
function GTK_OBJECT_TYPE(anObject: pointer) : GType;
function GTK_OBJECT_TYPE_NAME(anObject: pointer) : PGChar;

{ Macros for extracting the object_flags from GtkObject.
  }
function GTK_OBJECT_FLAGS(obj: pointer) : guint32;
function GTK_OBJECT_FLOATING(obj: pointer) : gboolean;

{ Macros for setting and clearing bits in the object_flags field of GtkObject.
  }
procedure GTK_OBJECT_SET_FLAGS(obj: pointer; flag: guint32);
procedure GTK_OBJECT_UNSET_FLAGS(obj: pointer; flag: guint32);

{ Application-level methods  }

function gtk_object_get_type:TGtkType; cdecl; external gtklib;
function gtk_object_new(_type:TGtkType; first_property_name:Pgchar; args:array of const):PGtkObject; cdecl; overload; external gtklib;
function gtk_object_new(_type:TGtkType; first_property_name:Pgchar):PGtkObject; cdecl; overload; varargs; external gtklib;
procedure gtk_object_sink(anObject:PGtkObject); cdecl; external gtklib;
procedure gtk_object_destroy(anObject:PGtkObject); cdecl; external gtklib;
{                                                               }

{$ifndef GTK_DISABLE_DEPRECATED }
function gtk_object_ref(anObject:PGtkObject):PGtkObject; cdecl; external gtklib;
procedure gtk_object_unref(anObject:PGtkObject); cdecl; external gtklib;
procedure gtk_object_weakref(anObject:PGtkObject; notify:TGtkDestroyNotify; data:gpointer); cdecl; external gtklib;
procedure gtk_object_weakunref(anObject:PGtkObject; notify:TGtkDestroyNotify; data:gpointer); cdecl; external gtklib;
{ Set 'data' to the "object_data" field of the anObject. The
    data is indexed by the "key". If there is already data
    associated with "key" then the new data will replace it.
    If 'data' is NULL then this call is equivalent to
    'gtk_object_remove_data'.
    The gtk_object_set_data_full variant acts just the same,
    but takes an additional argument which is a function to
    be called when the data is removed.
    `gtk_object_remove_data' is equivalent to the above,
    where 'data' is NULL
    `gtk_object_get_data' gets the data associated with "key".
  }
procedure gtk_object_set_data(anObject:PGtkObject; key:Pgchar; data:gpointer); cdecl; external gtklib;
procedure gtk_object_set_data_full(anObject:PGtkObject; key:Pgchar; data:gpointer; destroy:TGtkDestroyNotify); cdecl; external gtklib;
procedure gtk_object_remove_data(anObject:PGtkObject; key:Pgchar); cdecl; external gtklib;
function gtk_object_get_data(anObject:PGtkObject; key:Pgchar):gpointer; cdecl; external gtklib;
procedure gtk_object_remove_no_notify(anObject:PGtkObject; key:Pgchar); cdecl; external gtklib;
{ Set/get the "user_data" anObject data field of "anObject". It should
    be noted that these functions are no different than calling
    `gtk_object_set_data'/`gtk_object_get_data' with a key of "user_data".
    They are merely provided as a convenience.
  }
procedure gtk_object_set_user_data(anObject:PGtkObject; data:gpointer); cdecl; external gtklib;
function gtk_object_get_user_data(anObject:PGtkObject):gpointer; cdecl; external gtklib;
{ Object-level methods  }
{ Object data method variants that operate on key ids.  }
procedure gtk_object_set_data_by_id(anObject:PGtkObject; data_id:TGQuark; data:gpointer); cdecl; external gtklib;
procedure gtk_object_set_data_by_id_full(anObject:PGtkObject; data_id:TGQuark; data:gpointer; destroy:TGtkDestroyNotify); cdecl; external gtklib;
function gtk_object_get_data_by_id(anObject:PGtkObject; data_id:TGQuark):gpointer; cdecl; external gtklib;
procedure gtk_object_remove_data_by_id(anObject:PGtkObject; data_id:TGQuark); cdecl; external gtklib;
procedure gtk_object_remove_no_notify_by_id(anObject:PGtkObject; key_id:TGQuark); cdecl; external gtklib;

function gtk_object_data_try_key(_string:Pgchar):TGQuark;
function gtk_object_data_force_id(_string:Pgchar):TGQuark;

const
   GTK_ARG_READWRITE = GTK_ARG_READABLE or GTK_ARG_WRITABLE;

procedure gtk_object_get(anObject:PGtkObject; first_property_name:Pgchar; args:array of const); cdecl; overload; external gtklib;
procedure gtk_object_get(anObject:PGtkObject; first_property_name:Pgchar); cdecl; overload; varargs; external gtklib;
procedure gtk_object_set(anObject:PGtkObject; first_property_name:Pgchar; args:array of const); cdecl; overload; external gtklib;
procedure gtk_object_set(anObject:PGtkObject; first_property_name:Pgchar); cdecl; overload; varargs; external gtklib;
procedure gtk_object_add_arg_type(arg_name:Pgchar; arg_type:TGtkType; arg_flags:guint; arg_id:guint); cdecl; external gtklib;
{$endif}
{ GTK_DISABLE_DEPRECATED  }


{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_OBJECT : GType;
begin
   GTK_TYPE_OBJECT:=gtk_object_get_type;
end;

function GTK_OBJECT(anObject: pointer) : PGtkObject;
begin
   GTK_OBJECT:=PGtkObject(GTK_CHECK_CAST(anObject,GTK_TYPE_OBJECT));
end;

function GTK_OBJECT_CLASS(klass: pointer) : PGtkObjectClass;
begin
   GTK_OBJECT_CLASS:=PGtkObjectClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_OBJECT));
end;

function GTK_IS_OBJECT(anObject: pointer) : boolean;
begin
   GTK_IS_OBJECT:=GTK_CHECK_TYPE(anObject,GTK_TYPE_OBJECT);
end;

function GTK_IS_OBJECT_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_OBJECT_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_OBJECT);
end;

function GTK_OBJECT_GET_CLASS(anObject: pointer) : PGtkObjectClass;
begin
   GTK_OBJECT_GET_CLASS:=PGtkObjectClass(GTK_CHECK_GET_CLASS(anObject,GTK_TYPE_OBJECT));
end;

function GTK_OBJECT_TYPE(anObject: pointer) : GType;
begin
   GTK_OBJECT_TYPE:=G_TYPE_FROM_INSTANCE(anObject);
end;

function GTK_OBJECT_TYPE_NAME(anObject: pointer) : PGChar;
begin
   GTK_OBJECT_TYPE_NAME:=g_type_name(GTK_OBJECT_TYPE(anObject));
end;

function GTK_OBJECT_FLAGS(obj: pointer) : guint32;
begin
   GTK_OBJECT_FLAGS:=(GTK_OBJECT(obj))^.flags;
end;

function GTK_OBJECT_FLOATING(obj: pointer) : gboolean;
begin
   GTK_OBJECT_FLOATING:=((GTK_OBJECT_FLAGS(obj)) and GTK_FLOATING) <> 0;
end;

procedure GTK_OBJECT_SET_FLAGS(obj: pointer; flag: guint32);
begin
  GTK_OBJECT(obj)^.flags:=GTK_OBJECT(obj)^.flags or flag;
end;

procedure GTK_OBJECT_UNSET_FLAGS(obj: pointer; flag: guint32);
begin
  GTK_OBJECT(obj)^.flags:=GTK_OBJECT(obj)^.flags and not flag;
end;

function gtk_object_data_try_key(_string:Pgchar):TGQuark;
begin
   gtk_object_data_try_key:=g_quark_try_string(_string);
end;

function gtk_object_data_force_id(_string:Pgchar):TGQuark;
begin
   gtk_object_data_force_id:=g_quark_from_string(_string);
end;
{$ENDIF read_implementation}
// included by gtk2.pas

