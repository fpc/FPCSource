{$IfDef read_interface}

type
   PVT_SELTYPE = ^TVT_SELTYPE;
   TVT_SELTYPE = (VT_SELTYPE_NONE := 0,VT_SELTYPE_CHAR,VT_SELTYPE_WORD,VT_SELTYPE_LINE,VT_SELTYPE_MAGIC);

const
   VT_SELTYPE_MASK = $ff;
   VT_SELTYPE_BYEND = $8000;
   VT_SELTYPE_BYSTART = $4000;
   VT_SELTYPE_MOVED = $2000;

type
   PVT_SCROLLTYPE = ^TVT_SCROLLTYPE;
   TVT_SCROLLTYPE = (VT_SCROLL_ALWAYS := 0,VT_SCROLL_SOMETIMES,VT_SCROLL_NEVER);

   Pvt_magic_match = ^Tvt_magic_match;
   Pvt_match = ^Tvt_match;
   Pvt_match_block = ^Tvt_match_block;

   Pvtx = ^Tvtx;
   Tvtx = record
        vt : Tvt_em;
        flag0 : word;
        selection_data : Puint32;
        selection_size : longint;
        wordclass : array[0..31] of byte;
        selected : longint;
        selectiontype : TVT_SELTYPE;
        selstartx : longint;
        selstarty : longint;
        selendx : longint;
        selendy : longint;
        selstartxold : longint;
        selstartyold : longint;
        selendxold : longint;
        selendyold : longint;
        draw_text : procedure (user_data:pointer; line:Pvt_line; row:longint; col:longint; len:longint;
                      attr:longint);cdecl;
        scroll_area : procedure (user_data:pointer; firstrow:longint; count:longint; offset:longint; fill:longint); cdecl;
        cursor_state : function (user_data:pointer; state:longint):longint; cdecl;
        magic_list : Tvt_list;
        matches : Pvt_match;
        magic_matched : longint;
        match_shown : Pvt_match;
        scroll_type : byte;
     end;

   {$Warning (T)regex_t defined for (T)vt_magic_match may be wrong!!}
   Tregex_t = Record
     buffer : PByte;
     allocated : LongWord;
     sed : LongWord;
     syntax : LongWord;
     fastmap : PChar;
     translate : PChar;
     can_be_null : Word;
     regs_allocated : Word;
     fastmap_accurate : Word;
     no_sub : Word;
     not_bol : Word;
     not_eol : Word;
     newline_anchor : Word;
   end;

   Tvt_magic_match = record
        next : Pvt_magic_match;
        prev : Pvt_magic_match;
        regex : Pchar;
        preg : Tregex_t;
        highlight_mask : uint32;
        user_data : pointer;
     end;

   Tvt_match = record
        next : Pvt_match;
        match : Pvt_magic_match;
        matchstr : Pchar;
        blocks : Pvt_match_block;
     end;

   Tvt_match_block = record
        next : Pvt_match_block;
        line : Pvt_line;
        saveline : Pvt_line;
        lineno : dword;
        start : dword;
        theend : dword;
     end;

const
   bm_Tvtx_back_match = $1;
   bp_Tvtx_back_match = 0;
function back_match(var a : Tvtx) : dword;
procedure set_back_match(var a : Tvtx; __back_match : dword);

function vt_get_selection(vx:Pvtx; size:longint; len:Plongint):Pchar;cdecl;external libzvtdll name 'vt_get_selection';
procedure vt_clear_selection(vx:Pvtx);cdecl;external libzvtdll name 'vt_clear_selection';
procedure vt_fix_selection(vx:Pvtx);cdecl;external libzvtdll name 'vt_fix_selection';
procedure vt_draw_selection(vx:Pvtx);cdecl;external libzvtdll name 'vt_draw_selection';
procedure vt_update_rect(vx:Pvtx; fill:longint; sx:longint; sy:longint; ex:longint;
            ey:longint);cdecl;external libzvtdll name 'vt_update_rect';
procedure vt_update(vt:Pvtx; state:longint);cdecl;external libzvtdll name 'vt_update';
procedure vt_draw_cursor(vx:Pvtx; state:longint);cdecl;external libzvtdll name 'vt_draw_cursor';
procedure vt_set_wordclass(vx:Pvtx; s:Pbyte);cdecl;external libzvtdll name 'vt_set_wordclass';
function vt_get_attr_at(vx:Pvtx; col:longint; row:longint):longint;cdecl;external libzvtdll name 'vt_get_attr_at';
procedure vt_free_match_blocks(vx:Pvtx);cdecl;external libzvtdll name 'vt_free_match_blocks';
procedure vt_getmatches(vx:Pvtx);cdecl;external libzvtdll name 'vt_getmatches';
procedure vt_match_clear(vx:Pvtx; regex:Pchar);cdecl;external libzvtdll name 'vt_match_clear';
function vt_match_check(vx:Pvtx; x:longint; y:longint):Pvt_match;cdecl;external libzvtdll name 'vt_match_check';
procedure vt_match_highlight(vx:Pvtx; m:Pvt_match);cdecl;external libzvtdll name 'vt_match_highlight';
function vtx_new(width:longint; height:longint; user_data:pointer):Pvtx;cdecl;external libzvtdll name 'vtx_new';
procedure vtx_destroy(vx:Pvtx);cdecl;external libzvtdll name 'vtx_destroy';

{$EndIf read_interface}

{$Ifdef read_implementation}

function back_match(var a : Tvtx) : dword;
begin
   back_match:=(a.flag0 and bm_Tvtx_back_match) shr bp_Tvtx_back_match;
end;

procedure set_back_match(var a : Tvtx; __back_match : dword);
begin
   a.flag0:=a.flag0 or ((__back_match shl bp_Tvtx_back_match) and bm_Tvtx_back_match);
end;

{$Endif read_implementation}
