// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkEntryCompletionPrivate = pointer;

   PGtkEntryCompletion = ^TGtkEntryCompletion;
   TGtkEntryCompletion = record
        parent_instance : TGObject;
        priv : PGtkEntryCompletionPrivate;
     end;

   TGtkEntryCompletionMatchFunc = function (completion:PGtkEntryCompletion; key:Pgchar; iter:PGtkTreeIter; user_data:gpointer):gboolean;cdecl;


{ Padding for future expansion  }
   PGtkEntryCompletionClass = ^TGtkEntryCompletionClass;
   TGtkEntryCompletionClass = record
        parent_class : TGObjectClass;
        match_selected : function (completion:PGtkEntryCompletion; model:PGtkTreeModel; iter:PGtkTreeIter):gboolean;
        action_activated : procedure (completion:PGtkEntryCompletion; index:gint);
        _gtk_reserved0 : procedure ;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
     end;


{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_ENTRY_COMPLETION : GType;
function GTK_ENTRY_COMPLETION(obj : pointer) : PGtkEntryCompletion;
function GTK_ENTRY_COMPLETION_CLASS(klass : pointer) : PGtkEntryCompletionClass;
function GTK_IS_ENTRY_COMPLETION(obj : pointer) : gboolean;
function GTK_IS_ENTRY_COMPLETION_CLASS(klass :pointer) : gboolean;
function GTK_ENTRY_COMPLETION_GET_CLASS(obj : pointer) : PGtkEntryCompletionClass;


{ core  }

function gtk_entry_completion_get_type:GType;cdecl;external gtklib name 'gtk_entry_completion_get_type';

function gtk_entry_completion_new:PGtkEntryCompletion;cdecl;external gtklib name 'gtk_entry_completion_new';
function gtk_entry_completion_get_entry(completion:PGtkEntryCompletion):PGtkWidget;cdecl;external gtklib name 'gtk_entry_completion_get_entry';

procedure gtk_entry_completion_set_model(completion:PGtkEntryCompletion; model:PGtkTreeModel);cdecl;external gtklib name 'gtk_entry_completion_set_model';
function gtk_entry_completion_get_model(completion:PGtkEntryCompletion):PGtkTreeModel;cdecl;external gtklib name 'gtk_entry_completion_get_model';

procedure gtk_entry_completion_set_match_func(completion:PGtkEntryCompletion; func:TGtkEntryCompletionMatchFunc; func_data:gpointer; func_notify:TGDestroyNotify);cdecl;external gtklib name 'gtk_entry_completion_set_match_func';

procedure gtk_entry_completion_set_minimum_key_length(completion:PGtkEntryCompletion; length:gint);cdecl;external gtklib name 'gtk_entry_completion_set_minimum_key_length';
function gtk_entry_completion_get_minimum_key_length(completion:PGtkEntryCompletion):gint;cdecl;external gtklib name 'gtk_entry_completion_get_minimum_key_length';

procedure gtk_entry_completion_complete(completion:PGtkEntryCompletion);cdecl;external gtklib name 'gtk_entry_completion_complete';


procedure gtk_entry_completion_insert_action_text(completion:PGtkEntryCompletion; index:gint; text:Pgchar);cdecl;external gtklib name 'gtk_entry_completion_insert_action_text';
procedure gtk_entry_completion_insert_action_markup(completion:PGtkEntryCompletion; index:gint; markup:Pgchar);cdecl;external gtklib name 'gtk_entry_completion_insert_action_markup';
procedure gtk_entry_completion_delete_action(completion:PGtkEntryCompletion; index:gint);cdecl;external gtklib name 'gtk_entry_completion_delete_action';

{ convenience  }
procedure gtk_entry_completion_set_text_column(completion:PGtkEntryCompletion; column:gint);cdecl;external gtklib name 'gtk_entry_completion_set_text_column';

{$ifdef HasGTK2_6}
function gtk_entry_completion_get_inline_completion(completion:PGtkEntryCompletion):gboolean;cdecl;external gtklib;
function gtk_entry_completion_get_popup_completion(completion:PGtkEntryCompletion):gboolean;cdecl;external gtklib;
function gtk_entry_completion_get_text_column(completion:PGtkEntryCompletion):gint;cdecl;external gtklib;
procedure gtk_entry_completion_insert_prefix(completion:PGtkEntryCompletion);cdecl;external gtklib;
procedure gtk_entry_completion_set_inline_completion(completion:PGtkEntryCompletion; inline_completion:gboolean);cdecl;external gtklib;
procedure gtk_entry_completion_set_popup_completion(completion:PGtkEntryCompletion; popup_completion:gboolean);cdecl;external gtklib;
{$endif}

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_ENTRY_COMPLETION : GType;
begin
   GTK_TYPE_ENTRY_COMPLETION:=gtk_entry_completion_get_type;
end;

function GTK_ENTRY_COMPLETION(obj : pointer) : PGtkEntryCompletion;
begin
   GTK_ENTRY_COMPLETION:=PGtkEntryCompletion(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_ENTRY_COMPLETION));
end;

function GTK_ENTRY_COMPLETION_CLASS(klass : pointer) : PGtkEntryCompletionClass;
begin
   GTK_ENTRY_COMPLETION_CLASS:=PGtkEntryCompletionClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_ENTRY_COMPLETION));
end;

function GTK_IS_ENTRY_COMPLETION(obj : pointer) : gboolean;
begin
   GTK_IS_ENTRY_COMPLETION:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_ENTRY_COMPLETION);
end;

function GTK_IS_ENTRY_COMPLETION_CLASS(klass :pointer) : gboolean;
begin
   GTK_IS_ENTRY_COMPLETION_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_ENTRY_COMPLETION);
end;

function GTK_ENTRY_COMPLETION_GET_CLASS(obj : pointer) : PGtkEntryCompletionClass;
begin
   GTK_ENTRY_COMPLETION_GET_CLASS:=PGtkEntryCompletionClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_ENTRY_COMPLETION));
end;

{$endif read_implementation}
