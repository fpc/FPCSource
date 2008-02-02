{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

   type
       TGtkType = guint;
       PGtkType = ^TGtkType;

       PGtkArg = ^TGtkArg;
       PGtkTypeClass = ^TGtkTypeClass;
       PGtkObject = ^TGtkObject;
       PGtkObjectClass = ^TGtkObjectClass;

{***************************************
     Inserted from gtktypeutils
***************************************}

       TGtkClassInitFunc = procedure (klass:gpointer);cdecl;

       TGtkObjectInitFunc = procedure (theobject:gpointer; klass:gpointer);cdecl;

       TGtkSignalFunc = procedure ;cdecl;

       TGtkfunction = function (data:gpointer):gint;cdecl;

       TGtkDestroyNotify = procedure (data:gpointer);cdecl;

       TGtkCallbackMarshal = procedure (theobject:PGtkObject; data:gpointer; n_args:guint; args:PGtkArg);cdecl;

       TGtkSignalMarshaller = procedure (theobject:PGtkObject; func:TGtkSignalFunc; func_data:gpointer; args:PGtkArg);cdecl;

       TGtkArgGetFunc = procedure (_para1:PGtkObject; _para2:PGtkArg; _para3:guint);cdecl;

       TGtkArgSetFunc = procedure (_para1:PGtkObject; _para2:PGtkArg; _para3:guint);cdecl;

       GTK_SIGNAL_FUNC = TGtkSignalFunc;

       PGtkTypeObject = ^TGtkTypeObject;
       TGtkTypeObject = record
            klass : PGtkTypeClass;
         end;

       TGtkTypeClass = record
            thetype : TGtkType;
         end;

       TGtkArg = record
            thetype : TGtkType;
            name : Pgchar;
            d : record
                case longint of
                   0 : ( char_data : gchar );
                   1 : ( uchar_data : guchar );
                   2 : ( bool_data : gboolean );
                   3 : ( int_data : gint );
                   4 : ( uint_data : guint );
                   5 : ( long_data : glong );
                   6 : ( ulong_data : gulong );
                   7 : ( float_data : gfloat );
                   8 : ( double_data : gdouble );
                   9 : ( string_data : Pgchar );
                   10 : ( pointer_data : gpointer );
                   11 : ( object_data : PGtkObject );
                   12 : ( signal_data : record
                        f : TGtkSignalFunc;
                        d : gpointer;
                     end );
                   13 : ( args_data : record
                        n_args : gint;
                        args : PGtkArg;
                     end );
                   14 : ( callback_data : record
                        marshal : TGtkCallbackMarshal;
                        data : gpointer;
                        notify : TGtkDestroyNotify;
                     end );
                   15 : ( c_callback_data : record
                        func : TGtkfunction;
                        func_data : gpointer;
                     end );
                   16 : ( foreign_data : record
                        data : gpointer;
                        notify : TGtkDestroyNotify;
                     end );
                end;
         end;

{***************************************
         Inserted from gtkarg
***************************************}

     PGtkArgInfo = ^TGtkArgInfo;
     PPGtkArgInfo = ^PGtkArgInfo;
     TGtkArgInfo = record
          class_type : TGtkType;
          name : Pgchar;
          _type : TGtkType;
          arg_flags : guint;
          full_name : Pgchar;
          arg_id : guint;
          seq_id : guint;
       end;

{***************************************
          End of insertions
***************************************}

       TGtkObject = record
            klass : PGtkObjectClass;
            flags : guint32;
            ref_count : guint;
            object_data : PGData;
         end;

       TGtkObjectClass = record
            thetype : TGtkType;
            signals : Pguint;
            nsignals : guint;
            n_args : guint;
            construct_args : PGSList;
            set_arg : procedure (theobject:PGtkObject; arg:PGtkArg; arg_id:guint);cdecl;
            get_arg : procedure (theobject:PGtkObject; arg:PGtkArg; arg_id:guint);cdecl;
            shutdown : procedure (theobject:PGtkObject);cdecl;
            destroy : procedure (theobject:PGtkObject);cdecl;
            finalize : procedure (theobject:PGtkObject);cdecl;
         end;

     TGtkObjectFlags = longint;
const
     GTK_DESTROYED = 1 shl 0;
     GTK_FLOATING = 1 shl 1;
     GTK_CONNECTED = 1 shl 2;
     GTK_CONSTRUCTED = 1 shl 3;

Type
  GTK_OBJECT = PGtkObject;
  GTK_OBJECT_CLASS = PGtkObjectClass;

function  GTK_OBJECT_TYPE(obj : PGtkobject) : TGtkType;
function  GTK_OBJECT_SIGNALS(obj : PGtkobject) : Pguint;
function  GTK_OBJECT_NSIGNALS(obj : PGtkobject) : guint;
function  GTK_OBJECT_FLAGS(obj : PGtkobject) : longint;
function  GTK_OBJECT_DESTROYED(obj : PGtkobject) : boolean;
function  GTK_OBJECT_FLOATING(obj : PGtkobject) : boolean;
function  GTK_OBJECT_CONNECTED(obj : PGtkobject) : boolean;
function  GTK_OBJECT_IS_CONSTRUCTED(obj : PGtkobject) : boolean;

function  GTK_IS_OBJECT(obj:pointer):boolean;
function  GTK_IS_OBJECT_CLASS(klass:pointer):boolean;

function  gtk_object_get_type:TGtkType;cdecl;external gtkdll name 'gtk_object_get_type';
function  gtk_object_class_user_signal_new(klass:PGtkObjectClass; name:Pgchar; signal_flags:TGtkSignalRunType; marshaller:TGtkSignalMarshaller; return_val:TGtkType; nparams:guint; args:array of const):guint;cdecl;external gtkdll name 'gtk_object_class_user_signal_new';
function  gtk_object_class_user_signal_newv(klass:PGtkObjectClass; name:Pgchar; signal_flags:TGtkSignalRunType; marshaller:TGtkSignalMarshaller; return_val:TGtkType; nparams:guint; params:PGtkType):guint;cdecl;external gtkdll name 'gtk_object_class_user_signal_newv';
function  gtk_object_new(thetype:TGtkType; first_arg_name:Pgchar; args:array of const):PGtkObject;cdecl;external gtkdll name 'gtk_object_new';
function  gtk_object_newv(theobject_type:TGtkType; n_args:guint; args:PGtkArg):PGtkObject;cdecl;external gtkdll name 'gtk_object_newv';
procedure gtk_object_default_construct(theobject:PGtkObject);cdecl;external gtkdll name 'gtk_object_default_construct';
procedure gtk_object_constructed(theobject:PGtkObject);cdecl;external gtkdll name 'gtk_object_constructed';
procedure gtk_object_sink(theobject:PGtkObject);cdecl;external gtkdll name 'gtk_object_sink';
procedure gtk_object_ref(theobject:PGtkObject);cdecl;external gtkdll name 'gtk_object_ref';
procedure gtk_object_unref(theobject:PGtkObject);cdecl;external gtkdll name 'gtk_object_unref';
procedure gtk_object_weakref(theobject:PGtkObject; notify:TGtkDestroyNotify; data:gpointer);cdecl;external gtkdll name 'gtk_object_weakref';
procedure gtk_object_weakunref(theobject:PGtkObject; notify:TGtkDestroyNotify; data:gpointer);cdecl;external gtkdll name 'gtk_object_weakunref';
procedure gtk_object_destroy(theobject:PGtkObject);cdecl;external gtkdll name 'gtk_object_destroy';
procedure gtk_object_getv(theobject:PGtkObject; n_args:guint; args:PGtkArg);cdecl;external gtkdll name 'gtk_object_getv';
procedure gtk_object_get(theobject:PGtkObject; first_arg_name:Pgchar; args:array of const);cdecl;external gtkdll name 'gtk_object_get';
procedure gtk_object_set(theobject:PGtkObject; first_arg_name:Pgchar; args:array of const);cdecl;external gtkdll name 'gtk_object_set';
procedure gtk_object_setv(theobject:PGtkObject; n_args:guint; args:PGtkArg);cdecl;external gtkdll name 'gtk_object_setv';
function  gtk_object_query_args(class_type:TGtkType; arg_flags:PPguint32; n_args:Pguint):PGtkArg;cdecl;external gtkdll name 'gtk_object_query_args';
procedure gtk_object_set_data(theobject:PGtkObject; key:Pgchar; data:gpointer);cdecl;external gtkdll name 'gtk_object_set_data';
procedure gtk_object_set_data_full(theobject:PGtkObject; key:Pgchar; data:gpointer; destroy:TGtkDestroyNotify);cdecl;external gtkdll name 'gtk_object_set_data_full';
procedure gtk_object_remove_data(theobject:PGtkObject; key:Pgchar);cdecl;external gtkdll name 'gtk_object_remove_data';
function  gtk_object_get_data(theobject:PGtkObject; key:Pgchar):gpointer;cdecl;external gtkdll name 'gtk_object_get_data';
procedure gtk_object_remove_no_notify(theobject:PGtkObject; key:Pgchar);cdecl;external gtkdll name 'gtk_object_remove_no_notify';
procedure gtk_object_set_user_data(theobject:PGtkObject; data:gpointer);cdecl;external gtkdll name 'gtk_object_set_user_data';
function  gtk_object_get_user_data(theobject:PGtkObject):gpointer;cdecl;external gtkdll name 'gtk_object_get_user_data';
procedure gtk_object_class_add_signals(klass:PGtkObjectClass; signals:Pguint; nsignals:guint);cdecl;external gtkdll name 'gtk_object_class_add_signals';
procedure gtk_object_add_arg_type(arg_name:Pgchar; arg_type:TGtkType; arg_flags:guint; arg_id:guint);cdecl;external gtkdll name 'gtk_object_add_arg_type';
procedure gtk_object_set_data_by_id(theobject:PGtkObject; data_id:TGQuark; data:gpointer);cdecl;external gtkdll name 'gtk_object_set_data_by_id';
procedure gtk_object_set_data_by_id_full(theobject:PGtkObject; data_id:TGQuark; data:gpointer; destroy:TGtkDestroyNotify);cdecl;external gtkdll name 'gtk_object_set_data_by_id_full';
function  gtk_object_get_data_by_id(theobject:PGtkObject; data_id:TGQuark):gpointer;cdecl;external gtkdll name 'gtk_object_get_data_by_id';
procedure gtk_object_remove_data_by_id(theobject:PGtkObject; data_id:TGQuark);cdecl;external gtkdll name 'gtk_object_remove_data_by_id';
procedure gtk_object_remove_no_notify_by_id(theobject:PGtkObject; key_id:TGQuark);cdecl;external gtkdll name 'gtk_object_remove_no_notify_by_id';
procedure gtk_object_arg_set(theobject:PGtkObject; arg:PGtkArg; info:PGtkArgInfo);cdecl;external gtkdll name 'gtk_object_arg_set';
procedure gtk_object_arg_get(theobject:PGtkObject; arg:PGtkArg; info:PGtkArgInfo);cdecl;external gtkdll name 'gtk_object_arg_get';
function  gtk_object_args_collect(theobject_type:TGtkType; arg_list_p:PPGSList; info_list_p:PPGSList; first_arg_name:Pgchar; var_args:array of const):Pgchar;cdecl;external gtkdll name 'gtk_object_args_collect';
function  gtk_object_arg_get_info(theobject_type:TGtkType; arg_name:Pgchar; info_p:PPGtkArgInfo):Pgchar;cdecl;external gtkdll name 'gtk_object_arg_get_info';
procedure gtk_trace_referencing(theobject:PGtkObject; func:Pgchar; dummy:guint; line:guint; do_ref:gboolean);cdecl;external gtkdll name 'gtk_trace_referencing';

{    const
       gtk_object_data_try_key = g_quark_try_string;
       gtk_object_data_force_id = g_quark_from_string; }

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

    function GTK_OBJECT_TYPE(obj : PGtkobject) : TGtkType;
      begin
        GTK_OBJECT_TYPE:=obj^.klass^.thetype;
      end;

    function GTK_OBJECT_SIGNALS(obj : PGtkobject) : Pguint;
      begin
         GTK_OBJECT_SIGNALS:=obj^.klass^.signals;
      end;

    function GTK_OBJECT_NSIGNALS(obj : PGtkobject) : guint;
      begin
         GTK_OBJECT_NSIGNALS:=obj^.klass^.nsignals;
      end;

    function GTK_OBJECT_FLAGS(obj : PGtkobject) : longint;
      begin
         GTK_OBJECT_FLAGS:=obj^.flags;
      end;

    function GTK_OBJECT_DESTROYED(obj : PGtkobject) : boolean;
      begin
         GTK_OBJECT_DESTROYED:=(GTK_OBJECT_FLAGS(obj)) and longint(GTK_DESTROYED)<>0;
      end;

    function GTK_OBJECT_FLOATING(obj : PGtkobject) : boolean;
      begin
         GTK_OBJECT_FLOATING:=(GTK_OBJECT_FLAGS(obj)) and longint(GTK_FLOATING)<>0;
      end;

    function GTK_OBJECT_CONNECTED(obj : PGtkobject) : boolean;
      begin
         GTK_OBJECT_CONNECTED:=((GTK_OBJECT_FLAGS(obj)) and longint(GTK_CONNECTED)) <> 0;
      end;

    function GTK_OBJECT_IS_CONSTRUCTED(obj : PGtkobject) : boolean;
      begin
         GTK_OBJECT_IS_CONSTRUCTED:=((GTK_OBJECT_FLAGS(obj)) and longint(GTK_CONSTRUCTED)) <> 0;
       end;

function  GTK_IS_OBJECT(obj:pointer):boolean;
begin
  GTK_IS_OBJECT:=(obj<>nil) and GTK_IS_OBJECT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_OBJECT_CLASS(klass:pointer):boolean;
begin
  GTK_IS_OBJECT_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=longint(GTK_TYPE_OBJECT));
end;

{$endif read_implementation}


