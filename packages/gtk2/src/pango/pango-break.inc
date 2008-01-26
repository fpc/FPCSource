// included by pango.pp
{$IFDEF read_interface_types}

{ Logical attributes of a character.
  }
{ Can break line in front of character  }
{ Must break line in front of character  }
{ Can break here when doing char wrap  }
{ Whitespace character  }
{ cursor can appear in front of character (i.e. this is a grapheme
     boundary, or the first character in the text)
    }
{ Note that in degenerate cases, you could have both start/theEnd set on
     some text, most likely for sentences (e.g. no space after a period, so
     the next sentence starts right away)
    }
{ first character in a word  }
{ is first non-word char after a word  }
{ There are two ways to divide sentences. The first assigns all
     intersentence whitespace/control/format chars to some sentence,
     so all chars are in some sentence; is_sentence_boundary denotes
     the boundaries there. The second way doesn't assign
     between-sentence spaces, etc. to any sentence, so
     is_sentence_start/is_sentence_end mark the boundaries of those
     sentences.
    }
{ first character in a sentence  }
{ first non-sentence char after a sentence  }
   PPangoLogAttr = ^TPangoLogAttr;
   TPangoLogAttr = record
        flag0 : word;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_functions}
const
   bm_TPangoLogAttr_is_line_break = $1;
   bp_TPangoLogAttr_is_line_break = 0;
   bm_TPangoLogAttr_is_mandatory_break = $2;
   bp_TPangoLogAttr_is_mandatory_break = 1;
   bm_TPangoLogAttr_is_char_break = $4;
   bp_TPangoLogAttr_is_char_break = 2;
   bm_TPangoLogAttr_is_white = $8;
   bp_TPangoLogAttr_is_white = 3;
   bm_TPangoLogAttr_is_cursor_position = $10;
   bp_TPangoLogAttr_is_cursor_position = 4;
   bm_TPangoLogAttr_is_word_start = $20;
   bp_TPangoLogAttr_is_word_start = 5;
   bm_TPangoLogAttr_is_word_end = $40;
   bp_TPangoLogAttr_is_word_end = 6;
   bm_TPangoLogAttr_is_sentence_boundary = $80;
   bp_TPangoLogAttr_is_sentence_boundary = 7;
   bm_TPangoLogAttr_is_sentence_start = $100;
   bp_TPangoLogAttr_is_sentence_start = 8;
   bm_TPangoLogAttr_is_sentence_end = $200;
   bp_TPangoLogAttr_is_sentence_end = 9;

function is_line_break(var a : TPangoLogAttr) : guint;
procedure set_is_line_break(var a : TPangoLogAttr; __is_line_break : guint);
function is_mandatory_break(var a : TPangoLogAttr) : guint;
procedure set_is_mandatory_break(var a : TPangoLogAttr; __is_mandatory_break : guint);
function is_char_break(var a : TPangoLogAttr) : guint;
procedure set_is_char_break(var a : TPangoLogAttr; __is_char_break : guint);
function is_white(var a : TPangoLogAttr) : guint;
procedure set_is_white(var a : TPangoLogAttr; __is_white : guint);
function is_cursor_position(var a : TPangoLogAttr) : guint;
procedure set_is_cursor_position(var a : TPangoLogAttr; __is_cursor_position : guint);
function is_word_start(var a : TPangoLogAttr) : guint;
procedure set_is_word_start(var a : TPangoLogAttr; __is_word_start : guint);
function is_word_end(var a : TPangoLogAttr) : guint;
procedure set_is_word_end(var a : TPangoLogAttr; __is_word_end : guint);
function is_sentence_boundary(var a : TPangoLogAttr) : guint;
procedure set_is_sentence_boundary(var a : TPangoLogAttr; __is_sentence_boundary : guint);
function is_sentence_start(var a : TPangoLogAttr) : guint;
procedure set_is_sentence_start(var a : TPangoLogAttr; __is_sentence_start : guint);
function is_sentence_end(var a : TPangoLogAttr) : guint;
procedure set_is_sentence_end(var a : TPangoLogAttr; __is_sentence_end : guint);
{ Determine information about cluster/word/line breaks in a string
   of Unicode text.
  }

procedure pango_break(text:Pgchar; length:longint; analysis:PPangoAnalysis; attrs:PPangoLogAttr; attrs_len:longint); cdecl; external pangolib;
procedure pango_find_paragraph_boundary(text:Pgchar; length:gint; paragraph_delimiter_index:Pgint; next_paragraph_start:Pgint); cdecl; external pangolib;
procedure pango_get_log_attrs(text:Pchar; length:longint; level:longint; language:PPangoLanguage; log_attrs:PPangoLogAttr;
            attrs_len:longint); cdecl; external pangolib;
{$ifdef PANGO_ENABLE_ENGINE}
{ This is the default break algorithm, used if no language
   engine overrides it. Normally you should use pango_break()
   instead; this function is mostly useful for chaining up
   from a language engine override.
  }

procedure pango_default_break(text:Pgchar; length:longint; analysis:PPangoAnalysis; attrs:PPangoLogAttr; attrs_len:longint); cdecl; external pangolib;
{$endif PANGO_ENABLE_ENGINE}

{$endif read_interface_functions}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function is_line_break(var a : TPangoLogAttr) : guint;
begin
  is_line_break:=(a.flag0 and bm_TPangoLogAttr_is_line_break) shr bp_TPangoLogAttr_is_line_break;
end;

procedure set_is_line_break(var a : TPangoLogAttr; __is_line_break : guint);
begin
  a.flag0:=a.flag0 or ((__is_line_break shl bp_TPangoLogAttr_is_line_break) and bm_TPangoLogAttr_is_line_break);
end;

function is_mandatory_break(var a : TPangoLogAttr) : guint;
begin
  is_mandatory_break:=(a.flag0 and bm_TPangoLogAttr_is_mandatory_break) shr bp_TPangoLogAttr_is_mandatory_break;
end;

procedure set_is_mandatory_break(var a : TPangoLogAttr; __is_mandatory_break : guint);
begin
  a.flag0:=a.flag0 or ((__is_mandatory_break shl bp_TPangoLogAttr_is_mandatory_break) and bm_TPangoLogAttr_is_mandatory_break);
end;

function is_char_break(var a : TPangoLogAttr) : guint;
begin
  is_char_break:=(a.flag0 and bm_TPangoLogAttr_is_char_break) shr bp_TPangoLogAttr_is_char_break;
end;

procedure set_is_char_break(var a : TPangoLogAttr; __is_char_break : guint);
begin
  a.flag0:=a.flag0 or ((__is_char_break shl bp_TPangoLogAttr_is_char_break) and bm_TPangoLogAttr_is_char_break);
end;

function is_white(var a : TPangoLogAttr) : guint;
begin
  is_white:=(a.flag0 and bm_TPangoLogAttr_is_white) shr bp_TPangoLogAttr_is_white;
end;

procedure set_is_white(var a : TPangoLogAttr; __is_white : guint);
begin
  a.flag0:=a.flag0 or ((__is_white shl bp_TPangoLogAttr_is_white) and bm_TPangoLogAttr_is_white);
end;

function is_cursor_position(var a : TPangoLogAttr) : guint;
begin
  is_cursor_position:=(a.flag0 and bm_TPangoLogAttr_is_cursor_position) shr bp_TPangoLogAttr_is_cursor_position;
end;

procedure set_is_cursor_position(var a : TPangoLogAttr; __is_cursor_position : guint);
begin
  a.flag0:=a.flag0 or ((__is_cursor_position shl bp_TPangoLogAttr_is_cursor_position) and bm_TPangoLogAttr_is_cursor_position);
end;

function is_word_start(var a : TPangoLogAttr) : guint;
begin
  is_word_start:=(a.flag0 and bm_TPangoLogAttr_is_word_start) shr bp_TPangoLogAttr_is_word_start;
end;

procedure set_is_word_start(var a : TPangoLogAttr; __is_word_start : guint);
begin
  a.flag0:=a.flag0 or ((__is_word_start shl bp_TPangoLogAttr_is_word_start) and bm_TPangoLogAttr_is_word_start);
end;

function is_word_end(var a : TPangoLogAttr) : guint;
begin
  is_word_end:=(a.flag0 and bm_TPangoLogAttr_is_word_end) shr bp_TPangoLogAttr_is_word_end;
end;

procedure set_is_word_end(var a : TPangoLogAttr; __is_word_end : guint);
begin
  a.flag0:=a.flag0 or ((__is_word_end shl bp_TPangoLogAttr_is_word_end) and bm_TPangoLogAttr_is_word_end);
end;

function is_sentence_boundary(var a : TPangoLogAttr) : guint;
begin
  is_sentence_boundary:=(a.flag0 and bm_TPangoLogAttr_is_sentence_boundary) shr bp_TPangoLogAttr_is_sentence_boundary;
end;

procedure set_is_sentence_boundary(var a : TPangoLogAttr; __is_sentence_boundary : guint);
begin
  a.flag0:=a.flag0 or ((__is_sentence_boundary shl bp_TPangoLogAttr_is_sentence_boundary) and bm_TPangoLogAttr_is_sentence_boundary);
end;

function is_sentence_start(var a : TPangoLogAttr) : guint;
begin
  is_sentence_start:=(a.flag0 and bm_TPangoLogAttr_is_sentence_start) shr bp_TPangoLogAttr_is_sentence_start;
end;

procedure set_is_sentence_start(var a : TPangoLogAttr; __is_sentence_start : guint);
begin
  a.flag0:=a.flag0 or ((__is_sentence_start shl bp_TPangoLogAttr_is_sentence_start) and bm_TPangoLogAttr_is_sentence_start);
end;

function is_sentence_end(var a : TPangoLogAttr) : guint;
begin
  is_sentence_end:=(a.flag0 and bm_TPangoLogAttr_is_sentence_end) shr bp_TPangoLogAttr_is_sentence_end;
end;

procedure set_is_sentence_end(var a : TPangoLogAttr; __is_sentence_end : guint);
begin
  a.flag0:=a.flag0 or ((__is_sentence_end shl bp_TPangoLogAttr_is_sentence_end) and bm_TPangoLogAttr_is_sentence_end);
end;
{$ENDIF read_implementation}

