{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{$ifndef GTK_TEXT_TYPES_H}
{$define GTK_TEXT_TYPES_H}
{ C++ extern C conditionnal removed }
{ __cplusplus  }

{$include gtktextbuffer.inc}
{$include gtktexttagprivate.inc}
type
{
   Declarations for variables shared among the text-related files:
  }
{ In gtktextbtree.c  }
  var
     gtk_text_char_type : TGtkTextLineSegmentClass;cvar; external gtklib;
     gtk_text_toggle_on_type : TGtkTextLineSegmentClass;cvar; external gtklib;
     gtk_text_toggle_off_type : TGtkTextLineSegmentClass;cvar; external gtklib;
{ In gtktextmark.c  }
     gtk_text_left_mark_type : TGtkTextLineSegmentClass;cvar; external gtklib;
     gtk_text_right_mark_type : TGtkTextLineSegmentClass;cvar; external gtklib;
{ In gtktextchild.c  }
     gtk_text_pixbuf_type : TGtkTextLineSegmentClass;cvar; external gtklib;
     gtk_text_child_type : TGtkTextLineSegmentClass;cvar; external gtklib;
{
   UTF 8 Stubs
  }

const
   GTK_TEXT_UNKNOWN_CHAR = $FFFC;

  var
     gtk_text_unknown_char_utf8 : array of gchar;cvar;public;


function gtk_text_byte_begins_utf8_char(byte:Pgchar):gboolean; cdecl; external gtklib;
{ C++ end of extern C conditionnal removed }
{ __cplusplus  }
{$endif}

