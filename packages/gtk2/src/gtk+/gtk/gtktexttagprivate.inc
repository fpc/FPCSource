{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{$ifndef GTK_TEXT_TAG_PRIVATE_H}
{$define GTK_TEXT_TAG_PRIVATE_H}
{$include gtktexttag.inc}
type
{ values should already have desired defaults; this function will override
   the defaults with settings in the given tags, which should be sorted in
   ascending order of priority
 }

procedure _gtk_text_attributes_fill_from_tags(values:PGtkTextAttributes; tags:PPGtkTextTag; n_tags:guint); cdecl; external gtklib;
procedure _gtk_text_tag_array_sort(tag_array_p:PPGtkTextTag; len:guint); cdecl; external gtklib;
{ ensure colors are allocated, etc. for drawing  }
procedure _gtk_text_attributes_realize(values:PGtkTextAttributes; cmap:PGdkColormap; visual:PGdkVisual); cdecl; external gtklib;
{ free the stuff again  }
procedure _gtk_text_attributes_unrealize(values:PGtkTextAttributes; cmap:PGdkColormap; visual:PGdkVisual); cdecl; external gtklib;
function _gtk_text_tag_affects_size(tag:PGtkTextTag):gboolean; cdecl; external gtklib;
function _gtk_text_tag_affects_nonsize_appearance(tag:PGtkTextTag):gboolean; cdecl; external gtklib;
{$endif}

