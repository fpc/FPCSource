// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkAccelLabel = ^TGtkAccelLabel;
   TGtkAccelLabel = record
        _label : TGtkLabel;
        queue_id : guint;
        accel_padding : guint;
        accel_widget : PGtkWidget;
        accel_closure : PGClosure;
        accel_group : PGtkAccelGroup;
        accel_string : Pgchar;
        accel_string_width : guint16;
     end;

{ Padding for future expansion  }
   PGtkAccelLabelClass = ^TGtkAccelLabelClass;
   TGtkAccelLabelClass = record
        parent_class : TGtkLabelClass;
        signal_quote1 : Pgchar;
        signal_quote2 : Pgchar;
        mod_name_shift : Pgchar;
        mod_name_control : Pgchar;
        mod_name_alt : Pgchar;
        mod_separator : Pgchar;
        accel_seperator : Pgchar;
        flag0 : word;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

const
   bm_TGtkAccelLabelClass_latin1_to_char = $1;
   bp_TGtkAccelLabelClass_latin1_to_char = 0;

function GTK_TYPE_ACCEL_LABEL : GType;
function GTK_ACCEL_LABEL(obj: pointer) : PGtkAccelLabel;
function GTK_ACCEL_LABEL_CLASS(klass: pointer) : PGtkAccelLabelClass;
function GTK_IS_ACCEL_LABEL(obj: pointer) : boolean;
function GTK_IS_ACCEL_LABEL_CLASS(klass: pointer) : boolean;
function GTK_ACCEL_LABEL_GET_CLASS(obj: pointer) : PGtkAccelLabelClass;


function latin1_to_char(var a : TGtkAccelLabelClass) : guint;
procedure set_latin1_to_char(var a : TGtkAccelLabelClass; __latin1_to_char : guint);

function gtk_accel_label_get_type:TGtkType; cdecl; external gtklib;
function gtk_accel_label_new(_string:Pgchar):PGtkWidget; cdecl; external gtklib;
function gtk_accel_label_get_accel_widget(accel_label:PGtkAccelLabel):PGtkWidget; cdecl; external gtklib;
function gtk_accel_label_get_accel_width(accel_label:PGtkAccelLabel):guint; cdecl; external gtklib;
procedure gtk_accel_label_set_accel_widget(accel_label:PGtkAccelLabel; accel_widget:PGtkWidget); cdecl; external gtklib;
procedure gtk_accel_label_set_accel_closure(accel_label:PGtkAccelLabel; accel_closure:PGClosure); cdecl; external gtklib;
function gtk_accel_label_refetch(accel_label:PGtkAccelLabel):gboolean; cdecl; external gtklib;

{$ifndef GTK_DISABLE_DEPRECATED}
function gtk_accel_label_accelerator_width(accel_label:PGtkAccelLabel):guint;
{$endif}
{ GTK_DISABLE_DEPRECATED  }


{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_ACCEL_LABEL : GType;
begin
   GTK_TYPE_ACCEL_LABEL:=gtk_accel_label_get_type;
end;

function GTK_ACCEL_LABEL(obj: pointer) : PGtkAccelLabel;
begin
   GTK_ACCEL_LABEL:=PGtkAccelLabel(GTK_CHECK_CAST(obj,GTK_TYPE_ACCEL_LABEL));
end;

function GTK_ACCEL_LABEL_CLASS(klass: pointer) : PGtkAccelLabelClass;
begin
   GTK_ACCEL_LABEL_CLASS:=PGtkAccelLabelClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_ACCEL_LABEL));
end;

function GTK_IS_ACCEL_LABEL(obj: pointer) : boolean;
begin
   GTK_IS_ACCEL_LABEL:=GTK_CHECK_TYPE(obj,GTK_TYPE_ACCEL_LABEL);
end;

function GTK_IS_ACCEL_LABEL_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_ACCEL_LABEL_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_ACCEL_LABEL);
end;

function GTK_ACCEL_LABEL_GET_CLASS(obj: pointer) : PGtkAccelLabelClass;
begin
   GTK_ACCEL_LABEL_GET_CLASS:=PGtkAccelLabelClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_ACCEL_LABEL));
end;

function latin1_to_char(var a : TGtkAccelLabelClass) : guint;
begin
   latin1_to_char:=(a.flag0 and bm_TGtkAccelLabelClass_latin1_to_char) shr bp_TGtkAccelLabelClass_latin1_to_char;
end;

procedure set_latin1_to_char(var a : TGtkAccelLabelClass; __latin1_to_char : guint);
begin
   a.flag0:=a.flag0 or ((__latin1_to_char shl bp_TGtkAccelLabelClass_latin1_to_char) and bm_TGtkAccelLabelClass_latin1_to_char);
end;

function gtk_accel_label_accelerator_width(accel_label:PGtkAccelLabel):guint;
begin
  gtk_accel_label_accelerator_width:=gtk_accel_label_get_accel_width(accel_label);
end;
{$ENDIF read_implementation}
// included by gtk2.pas

