{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{$ifndef GTK_TEXT_ITER_PRIVATE_H}
{$define GTK_TEXT_ITER_PRIVATE_H}
{$include gtktextiter.inc}
{ C++ extern C conditionnal removed }
{ __cplusplus  }
{$include gtktextiter.inc}
{$include gtktextbtree.inc}


function _gtk_text_iter_get_indexable_segment(iter:PGtkTextIter):PGtkTextLineSegment; cdecl; external gtklib;

function _gtk_text_iter_get_any_segment(iter:PGtkTextIter):PGtkTextLineSegment; cdecl; external gtklib;

function _gtk_text_iter_get_text_line(iter:PGtkTextIter):PGtkTextLine; cdecl; external gtklib;

function _gtk_text_iter_get_btree(iter:PGtkTextIter):PGtkTextBTree; cdecl; external gtklib;
function _gtk_text_iter_forward_indexable_segment(iter:PGtkTextIter):gboolean; cdecl; external gtklib;
function _gtk_text_iter_backward_indexable_segment(iter:PGtkTextIter):gboolean; cdecl; external gtklib;

function _gtk_text_iter_get_segment_byte(iter:PGtkTextIter):gint; cdecl; external gtklib;

function _gtk_text_iter_get_segment_char(iter:PGtkTextIter):gint; cdecl; external gtklib;
{ debug  }

procedure _gtk_text_iter_check(iter:PGtkTextIter); cdecl; external gtklib;
{ C++ end of extern C conditionnal removed }
{ __cplusplus  }
{$endif}

