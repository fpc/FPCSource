// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkExpanderPrivate = pointer;

   PGtkExpander = ^TGtkExpander;
   TGtkExpander = record
        bin : TGtkBin;
        priv : PGtkExpanderPrivate;
     end;

{ Key binding signal; to get notification on the expansion
     state connect to notify:expanded.
}
   PGtkExpanderClass = ^TGtkExpanderClass;
   TGtkExpanderClass = record
        parent_class : TGtkBinClass;
        activate : procedure (expander:PGtkExpander);cdecl;
     end;

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_EXPANDER : GType;
function GTK_EXPANDER(obj : pointer) : PGtkExpander;
function GTK_EXPANDER_CLASS(klass : pointer) : PGtkExpanderClass;
function GTK_IS_EXPANDER(obj : pointer) : gboolean;
function GTK_IS_EXPANDER_CLASS(klass : pointer) : gboolean;
function GTK_EXPANDER_GET_CLASS(obj : pointer) : PGtkExpanderClass;


function gtk_expander_get_type:GType;cdecl;external gtklib name 'gtk_expander_get_type';

function gtk_expander_new(szLabel:Pgchar):PGtkWidget;cdecl;external gtklib name 'gtk_expander_new';
function gtk_expander_new_with_mnemonic(szLabel:Pgchar):PGtkWidget;cdecl;external gtklib name 'gtk_expander_new_with_mnemonic';
procedure gtk_expander_set_expanded(expander:PGtkExpander; expanded:gboolean);cdecl;external gtklib name 'gtk_expander_set_expanded';
function gtk_expander_get_expanded(expander:PGtkExpander):gboolean;cdecl;external gtklib name 'gtk_expander_get_expanded';


{ Spacing between the expander/label and the child  }
procedure gtk_expander_set_spacing(expander:PGtkExpander; spacing:gint);cdecl;external gtklib name 'gtk_expander_set_spacing';
function gtk_expander_get_spacing(expander:PGtkExpander):gint;cdecl;external gtklib name 'gtk_expander_get_spacing';

procedure gtk_expander_set_label(expander:PGtkExpander; szLabel:Pgchar);cdecl;external gtklib name 'gtk_expander_set_label';
function gtk_expander_get_label(expander:PGtkExpander):Pgchar;cdecl;external gtklib name 'gtk_expander_get_label';

procedure gtk_expander_set_use_underline(expander:PGtkExpander; use_underline:gboolean);cdecl;external gtklib name 'gtk_expander_set_use_underline';
function gtk_expander_get_use_underline(expander:PGtkExpander):gboolean;cdecl;external gtklib name 'gtk_expander_get_use_underline';

procedure gtk_expander_set_use_markup(expander:PGtkExpander; use_markup:gboolean);cdecl;external gtklib name 'gtk_expander_set_use_markup';
function gtk_expander_get_use_markup(expander:PGtkExpander):gboolean;cdecl;external gtklib name 'gtk_expander_get_use_markup';

procedure gtk_expander_set_label_widget(expander:PGtkExpander; label_widget:PGtkWidget);cdecl;external gtklib name 'gtk_expander_set_label_widget';
function gtk_expander_get_label_widget(expander:PGtkExpander):PGtkWidget;cdecl;external gtklib name 'gtk_expander_get_label_widget';

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_EXPANDER : GType;
begin
   GTK_TYPE_EXPANDER:=gtk_expander_get_type;
end;

function GTK_EXPANDER(obj : pointer) : PGtkExpander;
begin
   GTK_EXPANDER:=PGtkExpander(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_EXPANDER));
end;

function GTK_EXPANDER_CLASS(klass : pointer) : PGtkExpanderClass;
begin
   GTK_EXPANDER_CLASS:=PGtkExpanderClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_EXPANDER));
end;


function GTK_IS_EXPANDER(obj : pointer) : gboolean;
begin
   GTK_IS_EXPANDER:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_EXPANDER);
end;

function GTK_IS_EXPANDER_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_EXPANDER_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_EXPANDER);
end;

function GTK_EXPANDER_GET_CLASS(obj : pointer) : PGtkExpanderClass;
begin
   GTK_EXPANDER_GET_CLASS:=PGtkExpanderClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_EXPANDER));
end;

{$endif read_implementation}
