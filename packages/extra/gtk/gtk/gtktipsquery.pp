{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkTipsQuery = ^TGtkTipsQuery;
     TGtkTipsQuery = record
          thelabel : TGtkLabel;
          flag0 : {$ifdef win32}longint{$else}word{$endif};
          label_inactive : Pgchar;
          label_no_tip : Pgchar;
          caller : PGtkWidget;
          last_crossed : PGtkWidget;
          query_cursor : PGdkCursor;
       end;

  const
     bm_TGtkTipsQuery_emit_always = $1;
     bp_TGtkTipsQuery_emit_always = 0;
     bm_TGtkTipsQuery_in_query = $2;
     bp_TGtkTipsQuery_in_query = 1;
function  emit_always(var a : TGtkTipsQuery) : guint;
procedure set_emit_always(var a : TGtkTipsQuery; __emit_always : guint);
function  in_query(var a : TGtkTipsQuery) : guint;
procedure set_in_query(var a : TGtkTipsQuery; __in_query : guint);

  type
     PGtkTipsQueryClass = ^TGtkTipsQueryClass;
     TGtkTipsQueryClass = record
          parent_class : TGtkLabelClass;
          start_query : procedure (tips_query:PGtkTipsQuery); cdecl;
          stop_query : procedure (tips_query:PGtkTipsQuery); cdecl;
          widget_entered : procedure (tips_query:PGtkTipsQuery; widget:PGtkWidget; tip_text:Pgchar; tip_private:Pgchar); cdecl;
          widget_selected : function (tips_query:PGtkTipsQuery; widget:PGtkWidget; tip_text:Pgchar; tip_private:Pgchar; event:PGdkEventButton):gint; cdecl;
       end;

Type
  GTK_TIPS_QUERY=PGtkTipsQuery;
  GTK_TIPS_QUERY_CLASS=PGtkTipsQueryClass;

function  GTK_TIPS_QUERY_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_tips_query_get_type';
function  GTK_IS_TIPS_QUERY(obj:pointer):boolean;
function  GTK_IS_TIPS_QUERY_CLASS(klass:pointer):boolean;

function  gtk_tips_query_get_type:TGtkType;cdecl;external gtkdll name 'gtk_tips_query_get_type';
function  gtk_tips_query_new:PGtkWidget;cdecl;external gtkdll name 'gtk_tips_query_new';
procedure gtk_tips_query_start_query(tips_query:PGtkTipsQuery);cdecl;external gtkdll name 'gtk_tips_query_start_query';
procedure gtk_tips_query_stop_query(tips_query:PGtkTipsQuery);cdecl;external gtkdll name 'gtk_tips_query_stop_query';
procedure gtk_tips_query_set_caller(tips_query:PGtkTipsQuery; caller:PGtkWidget);cdecl;external gtkdll name 'gtk_tips_query_set_caller';
procedure gtk_tips_query_set_labels(tips_query:PGtkTipsQuery; label_inactive:Pgchar; label_no_tip:Pgchar);cdecl;external gtkdll name 'gtk_tips_query_set_labels';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  emit_always(var a : TGtkTipsQuery) : guint;
    begin
       emit_always:=(a.flag0 and bm_TGtkTipsQuery_emit_always) shr bp_TGtkTipsQuery_emit_always;
    end;

procedure set_emit_always(var a : TGtkTipsQuery; __emit_always : guint);
    begin
       a.flag0:=a.flag0 or ((__emit_always shl bp_TGtkTipsQuery_emit_always) and bm_TGtkTipsQuery_emit_always);
    end;

function  in_query(var a : TGtkTipsQuery) : guint;
    begin
       in_query:=(a.flag0 and bm_TGtkTipsQuery_in_query) shr bp_TGtkTipsQuery_in_query;
    end;

procedure set_in_query(var a : TGtkTipsQuery; __in_query : guint);
    begin
       a.flag0:=a.flag0 or ((__in_query shl bp_TGtkTipsQuery_in_query) and bm_TGtkTipsQuery_in_query);
    end;

function  GTK_IS_TIPS_QUERY(obj:pointer):boolean;
begin
  GTK_IS_TIPS_QUERY:=(obj<>nil) and GTK_IS_TIPS_QUERY_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_TIPS_QUERY_CLASS(klass:pointer):boolean;
begin
  GTK_IS_TIPS_QUERY_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_TIPS_QUERY_TYPE);
end;

{$endif read_implementation}


