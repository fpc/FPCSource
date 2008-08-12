// included by gdk2.pp

{$IFDEF read_interface_types}

{ Cursor types. }
   PGdkCursorType = ^TGdkCursorType;
   TGdkCursorType = gint;

   PGdkCursor = ^TGdkCursor;
   TGdkCursor = record
        _type : TGdkCursorType;
        ref_count : guint;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
     GDK_CURSOR_IS_PIXMAP = -(1);
     GDK_X_CURSOR = 0;
     GDK_ARROW = 2;
     GDK_BASED_ARROW_DOWN = 4;
     GDK_BASED_ARROW_UP = 6;
     GDK_BOAT = 8;
     GDK_BOGOSITY = 10;
     GDK_BOTTOM_LEFT_CORNER = 12;
     GDK_BOTTOM_RIGHT_CORNER = 14;
     GDK_BOTTOM_SIDE = 16;
     GDK_BOTTOM_TEE = 18;
     GDK_BOX_SPIRAL = 20;
     GDK_CENTER_PTR = 22;
     GDK_CIRCLE = 24;
     GDK_CLOCK = 26;
     GDK_COFFEE_MUG = 28;
     GDK_CROSS = 30;
     GDK_CROSS_REVERSE = 32;
     GDK_CROSSHAIR = 34;
     GDK_DIAMOND_CROSS = 36;
     GDK_DOT = 38;
     GDK_DOTBOX = 40;
     GDK_DOUBLE_ARROW = 42;
     GDK_DRAFT_LARGE = 44;
     GDK_DRAFT_SMALL = 46;
     GDK_DRAPED_BOX = 48;
     GDK_EXCHANGE = 50;
     GDK_FLEUR = 52;
     GDK_GOBBLER = 54;
     GDK_GUMBY = 56;
     GDK_HAND1 = 58;
     GDK_HAND2 = 60;
     GDK_HEART = 62;
     GDK_ICON = 64;
     GDK_IRON_CROSS = 66;
     GDK_LEFT_PTR = 68;
     GDK_LEFT_SIDE = 70;
     GDK_LEFT_TEE = 72;
     GDK_LEFTBUTTON = 74;
     GDK_LL_ANGLE = 76;
     GDK_LR_ANGLE = 78;
     GDK_MAN = 80;
     GDK_MIDDLEBUTTON = 82;
     GDK_MOUSE = 84;
     GDK_PENCIL = 86;
     GDK_PIRATE = 88;
     GDK_PLUS = 90;
     GDK_QUESTION_ARROW = 92;
     GDK_RIGHT_PTR = 94;
     GDK_RIGHT_SIDE = 96;
     GDK_RIGHT_TEE = 98;
     GDK_RIGHTBUTTON = 100;
     GDK_RTL_LOGO = 102;
     GDK_SAILBOAT = 104;
     GDK_SB_DOWN_ARROW = 106;
     GDK_SB_H_DOUBLE_ARROW = 108;
     GDK_SB_LEFT_ARROW = 110;
     GDK_SB_RIGHT_ARROW = 112;
     GDK_SB_UP_ARROW = 114;
     GDK_SB_V_DOUBLE_ARROW = 116;
     GDK_SHUTTLE = 118;
     GDK_SIZING = 120;
     GDK_SPIDER = 122;
     GDK_SPRAYCAN = 124;
     GDK_STAR = 126;
     GDK_TARGET = 128;
     GDK_TCROSS = 130;
     GDK_TOP_LEFT_ARROW = 132;
     GDK_TOP_LEFT_CORNER = 134;
     GDK_TOP_RIGHT_CORNER = 136;
     GDK_TOP_SIDE = 138;
     GDK_TOP_TEE = 140;
     GDK_TREK = 142;
     GDK_UL_ANGLE = 144;
     GDK_UMBRELLA = 146;
     GDK_UR_ANGLE = 148;
     GDK_WATCH = 150;
     GDK_XTERM = 152;
     GDK_LAST_CURSOR=GDK_XTERM+1;

function GDK_TYPE_CURSOR : GType;
function GDK_CURSOR(anObject : pointer) : PGdkCursor;
function GDK_IS_CURSOR(anObject : pointer) : boolean;

{ Cursors }

function gdk_cursor_get_type:GType; cdecl; external gdklib;
function gdk_cursor_new_for_display(display: PGdkDisplay; cursor_type:TGdkCursorType):PGdkCursor; cdecl; external gdklib;
function gdk_cursor_new_for_screen(screen:PGdkScreen; cursor_type:TGdkCursorType):PGdkCursor; cdecl; external gdklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_cursor_new(cursor_type:TGdkCursorType):PGdkCursor; cdecl; external gdklib;
{$endif}

function gdk_cursor_new_from_pixmap(source:PGdkPixmap; mask:PGdkPixmap; fg:PGdkColor; bg:PGdkColor; x:gint;
           y:gint):PGdkCursor; cdecl; external gdklib;
function gdk_cursor_get_display(cursor: PGdkCursor):PGdkDisplay; cdecl; external gdklib;
function gdk_cursor_get_screen(cursor:PGdkCursor):PGdkScreen; cdecl; external gdklib;
function gdk_cursor_ref(cursor:PGdkCursor):PGdkCursor; cdecl; external gdklib;
procedure gdk_cursor_unref(cursor:PGdkCursor); cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
procedure gdk_cursor_destroy(cursor:PGdkCursor);
{$endif}
{ GDK_DISABLE_DEPRECATED  }

{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
procedure gdk_cursor_destroy(cursor:PGdkCursor);
begin
  gdk_cursor_unref(cursor);
end;

function GDK_TYPE_CURSOR : GType;
begin
  GDK_TYPE_CURSOR:=gdk_cursor_get_type;
end;

function GDK_CURSOR(anObject : pointer) : PGdkCursor;
begin
  GDK_CURSOR:=PGdkCursor(G_TYPE_CHECK_INSTANCE_CAST(anObject,GDK_TYPE_CURSOR));
end;

function GDK_IS_CURSOR(anObject : pointer) : boolean;
begin
  GDK_IS_CURSOR:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_CURSOR);
end;
{$ENDIF}
