// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkIMContextSimple = ^TGtkIMContextSimple;
   TGtkIMContextSimple = record
        anObject : TGtkIMContext;
        tables : PGSList;
        compose_buffer : array[0..(GTK_MAX_COMPOSE_LEN + 1)-1] of guint;
        tentative_match : gunichar;
        tentative_match_len : gint;
        flag0 : word;
     end;

   PGtkIMContextSimpleClass = ^TGtkIMContextSimpleClass;
   TGtkIMContextSimpleClass = record
        parent_class : TGtkIMContextClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkIMContextSimple_in_hex_sequence = $1;
   bp_TGtkIMContextSimple_in_hex_sequence = 0;


function GTK_TYPE_IM_CONTEXT_SIMPLE : GType;
function GTK_IM_CONTEXT_SIMPLE(obj: pointer) : PGtkIMContextSimple;
function GTK_IM_CONTEXT_SIMPLE_CLASS(klass: pointer) : PGtkIMContextSimpleClass;
function GTK_IS_IM_CONTEXT_SIMPLE(obj: pointer) : boolean;
function GTK_IS_IM_CONTEXT_SIMPLE_CLASS(klass: pointer) : boolean;
function GTK_IM_CONTEXT_SIMPLE_GET_CLASS(obj: pointer) : PGtkIMContextSimpleClass;

function in_hex_sequence(var a : TGtkIMContextSimple) : guint;
procedure set_in_hex_sequence(var a : TGtkIMContextSimple; __in_hex_sequence : guint);

function gtk_im_context_simple_get_type:TGtkType; cdecl; external gtklib;
function gtk_im_context_simple_new:PGtkIMContext; cdecl; external gtklib;
procedure gtk_im_context_simple_add_table(context_simple:PGtkIMContextSimple; data:Pguint16; max_seq_len:gint; n_seqs:gint); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_IM_CONTEXT_SIMPLE : GType;
begin
   GTK_TYPE_IM_CONTEXT_SIMPLE:=gtk_im_context_simple_get_type;
end;

function GTK_IM_CONTEXT_SIMPLE(obj: pointer) : PGtkIMContextSimple;
begin
   GTK_IM_CONTEXT_SIMPLE:=PGtkIMContextSimple(GTK_CHECK_CAST(obj,GTK_TYPE_IM_CONTEXT_SIMPLE));
end;

function GTK_IM_CONTEXT_SIMPLE_CLASS(klass: pointer) : PGtkIMContextSimpleClass;
begin
   GTK_IM_CONTEXT_SIMPLE_CLASS:=PGtkIMContextSimpleClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_IM_CONTEXT_SIMPLE));
end;

function GTK_IS_IM_CONTEXT_SIMPLE(obj: pointer) : boolean;
begin
   GTK_IS_IM_CONTEXT_SIMPLE:=GTK_CHECK_TYPE(obj,GTK_TYPE_IM_CONTEXT_SIMPLE);
end;

function GTK_IS_IM_CONTEXT_SIMPLE_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_IM_CONTEXT_SIMPLE_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_IM_CONTEXT_SIMPLE);
end;

function GTK_IM_CONTEXT_SIMPLE_GET_CLASS(obj: pointer) : PGtkIMContextSimpleClass;
begin
   GTK_IM_CONTEXT_SIMPLE_GET_CLASS:=PGtkIMContextSimpleClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_IM_CONTEXT_SIMPLE));
end;

function in_hex_sequence(var a : TGtkIMContextSimple) : guint;
begin
   in_hex_sequence:=(a.flag0 and bm_TGtkIMContextSimple_in_hex_sequence) shr bp_TGtkIMContextSimple_in_hex_sequence;
end;

procedure set_in_hex_sequence(var a : TGtkIMContextSimple; __in_hex_sequence : guint);
begin
   a.flag0:=a.flag0 or ((__in_hex_sequence shl bp_TGtkIMContextSimple_in_hex_sequence) and bm_TGtkIMContextSimple_in_hex_sequence);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

