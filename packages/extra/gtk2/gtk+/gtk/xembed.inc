{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$PACKRECORDS C}

{ XEMBED messages  }

const
   XEMBED_EMBEDDED_NOTIFY = 0;
   XEMBED_WINDOW_ACTIVATE = 1;
   XEMBED_WINDOW_DEACTIVATE = 2;
   XEMBED_REQUEST_FOCUS = 3;
   XEMBED_FOCUS_IN = 4;
   XEMBED_FOCUS_OUT = 5;
   XEMBED_FOCUS_NEXT = 6;
   XEMBED_FOCUS_PREV = 7;
   XEMBED_GRAB_KEY = 8;
   XEMBED_UNGRAB_KEY = 9;
   XEMBED_MODALITY_ON = 10;
   XEMBED_MODALITY_OFF = 11;
{ Non standard messages }
   XEMBED_GTK_GRAB_KEY = 108;
   XEMBED_GTK_UNGRAB_KEY = 109;
{ Details for  XEMBED_FOCUS_IN:  }
   XEMBED_FOCUS_CURRENT = 0;
   XEMBED_FOCUS_FIRST = 1;
   XEMBED_FOCUS_LAST = 2;
{ Flags for _XEMBED_INFO  }
   XEMBED_MAPPED = 1 shl 0;

