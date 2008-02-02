// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{
   The data structure below defines line segments that represent
   marks.  There is one of these for each mark in the text.
  }
   PGtkTextMarkBody = ^TGtkTextMarkBody;
   TGtkTextMarkBody = record
        obj : PGtkTextMark;
        name : Pgchar;
        tree : PGtkTextBTree;
        line : PGtkTextLine;
        flag0 : word;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkTextMarkBody_visible = $1;
   bp_TGtkTextMarkBody_visible = 0;
   bm_TGtkTextMarkBody_not_deleteable = $2;
   bp_TGtkTextMarkBody_not_deleteable = 1;

function visible(var a : TGtkTextMarkBody) : guint; overload;
procedure set_visible(var a : TGtkTextMarkBody; __visible : guint); overload;
function not_deleteable(var a : TGtkTextMarkBody) : guint;
procedure set_not_deleteable(var a : TGtkTextMarkBody; __not_deleteable : guint);


function _gtk_mark_segment_new(tree:PGtkTextBTree; left_gravity:gboolean; name:Pgchar):PGtkTextLineSegment; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function visible(var a : TGtkTextMarkBody) : guint;
begin
   visible:=(a.flag0 and bm_TGtkTextMarkBody_visible) shr bp_TGtkTextMarkBody_visible;
end;

procedure set_visible(var a : TGtkTextMarkBody; __visible : guint);
begin
   a.flag0:=a.flag0 or ((__visible shl bp_TGtkTextMarkBody_visible) and bm_TGtkTextMarkBody_visible);
end;

function not_deleteable(var a : TGtkTextMarkBody) : guint;
begin
   not_deleteable:=(a.flag0 and bm_TGtkTextMarkBody_not_deleteable) shr bp_TGtkTextMarkBody_not_deleteable;
end;

procedure set_not_deleteable(var a : TGtkTextMarkBody; __not_deleteable : guint);
begin
   a.flag0:=a.flag0 or ((__not_deleteable shl bp_TGtkTextMarkBody_not_deleteable) and bm_TGtkTextMarkBody_not_deleteable);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

