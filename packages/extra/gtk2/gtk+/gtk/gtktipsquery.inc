// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ --- structures ---  }
   PGtkTipsQuery = ^TGtkTipsQuery;
   TGtkTipsQuery = record
        _label : TGtkLabel;
        flag0 : word;
        label_inactive : Pgchar;
        label_no_tip : Pgchar;
        caller : PGtkWidget;
        last_crossed : PGtkWidget;
        query_cursor : PGdkCursor;
     end;

{ Padding for future expansion  }
   PGtkTipsQueryClass = ^TGtkTipsQueryClass;
   TGtkTipsQueryClass = record
        parent_class : TGtkLabelClass;
        start_query : procedure (tips_query:PGtkTipsQuery); cdecl;
        stop_query : procedure (tips_query:PGtkTipsQuery); cdecl;
        widget_entered : procedure (tips_query:PGtkTipsQuery; widget:PGtkWidget; tip_text:Pgchar; tip_private:Pgchar); cdecl;
        widget_selected : function (tips_query:PGtkTipsQuery; widget:PGtkWidget; tip_text:Pgchar; tip_private:Pgchar; event:PGdkEventButton):gint; cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkTipsQuery_emit_always = $1;
   bp_TGtkTipsQuery_emit_always = 0;
   bm_TGtkTipsQuery_in_query = $2;
   bp_TGtkTipsQuery_in_query = 1;


function GTK_TYPE_TIPS_QUERY : GType;
function GTK_TIPS_QUERY(obj: pointer) : PGtkTipsQuery;
function GTK_TIPS_QUERY_CLASS(klass: pointer) : PGtkTipsQueryClass;
function GTK_IS_TIPS_QUERY(obj: pointer) : boolean;
function GTK_IS_TIPS_QUERY_CLASS(klass: pointer) : boolean;
function GTK_TIPS_QUERY_GET_CLASS(obj: pointer) : PGtkTipsQueryClass;

function emit_always(var a : TGtkTipsQuery) : guint;
procedure set_emit_always(var a : TGtkTipsQuery; __emit_always : guint);
function in_query(var a : TGtkTipsQuery) : guint;
procedure set_in_query(var a : TGtkTipsQuery; __in_query : guint);

{ --- prototypes ---  }

function gtk_tips_query_get_type:TGtkType; cdecl; external gtklib;
function gtk_tips_query_new:PGtkWidget; cdecl; external gtklib;
procedure gtk_tips_query_start_query(tips_query:PGtkTipsQuery); cdecl; external gtklib;
procedure gtk_tips_query_stop_query(tips_query:PGtkTipsQuery); cdecl; external gtklib;
procedure gtk_tips_query_set_caller(tips_query:PGtkTipsQuery; caller:PGtkWidget); cdecl; external gtklib;
procedure gtk_tips_query_set_labels(tips_query:PGtkTipsQuery; label_inactive:Pgchar; label_no_tip:Pgchar); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TIPS_QUERY : GType;
begin
   GTK_TYPE_TIPS_QUERY:=gtk_tips_query_get_type;
end;

function GTK_TIPS_QUERY(obj: pointer) : PGtkTipsQuery;
begin
   GTK_TIPS_QUERY:=PGtkTipsQuery(GTK_CHECK_CAST(obj,GTK_TYPE_TIPS_QUERY));
end;

function GTK_TIPS_QUERY_CLASS(klass: pointer) : PGtkTipsQueryClass;
begin
   GTK_TIPS_QUERY_CLASS:=PGtkTipsQueryClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TIPS_QUERY));
end;

function GTK_IS_TIPS_QUERY(obj: pointer) : boolean;
begin
   GTK_IS_TIPS_QUERY:=GTK_CHECK_TYPE(obj,GTK_TYPE_TIPS_QUERY);
end;

function GTK_IS_TIPS_QUERY_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TIPS_QUERY_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TIPS_QUERY);
end;

function GTK_TIPS_QUERY_GET_CLASS(obj: pointer) : PGtkTipsQueryClass;
begin
   GTK_TIPS_QUERY_GET_CLASS:=PGtkTipsQueryClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TIPS_QUERY));
end;

function emit_always(var a : TGtkTipsQuery) : guint;
begin
   emit_always:=(a.flag0 and bm_TGtkTipsQuery_emit_always) shr bp_TGtkTipsQuery_emit_always;
end;

procedure set_emit_always(var a : TGtkTipsQuery; __emit_always : guint);
begin
   a.flag0:=a.flag0 or ((__emit_always shl bp_TGtkTipsQuery_emit_always) and bm_TGtkTipsQuery_emit_always);
end;

function in_query(var a : TGtkTipsQuery) : guint;
begin
   in_query:=(a.flag0 and bm_TGtkTipsQuery_in_query) shr bp_TGtkTipsQuery_in_query;
end;

procedure set_in_query(var a : TGtkTipsQuery; __in_query : guint);
begin
   a.flag0:=a.flag0 or ((__in_query shl bp_TGtkTipsQuery_in_query) and bm_TGtkTipsQuery_in_query);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

