{
  $Id$

  GTK implementation for shedit
  Copyright (C) 1999  Sebastian Guenther (sguenther@gmx.de)

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}
unit gtkshedit;
interface

{$MODE objfpc}
{$H+}

uses
  SysUtils, Classes,
  GDK, GTK,
  doc_text, SHEdit;

const
  colBlack       = $000000;
  colDarkBlue    = $000080;
  colBlue        = $0000ff;
  colDarkGreen   = $008000;
  colGreen       = $00ff00;
  colDarkCyan    = $008080;
  colCyan        = $00ffff;
  colBrown       = $800000;
  colRed         = $ff0000;
  colDarkMagenta = $800080;
  colMagenta     = $ff00ff;
  colDarkYellow  = $808000;
  colYellow      = $ffff00;
  colGray        = $808080;
  colGrey        = colGray;
  colLightGray   = $c0c0c0;
  colLightGrey   = colLightGray;
  colWhite       = $ffffff;
  colInvalid     = $ff000000;
  colDefault     = $ffffffff;

type

  TSHFontStyle = (fsNormal, fsBold, fsItalics, fsBoldItalics);

  TSHStyle = record
    Name: String[32];
    Color, Background: LongWord;
    FontStyle: TSHFontStyle;
  end;

  TSHStyleArray = array[1..1] of TSHStyle;  // Notice the 1!
  PSHStyleArray = ^TSHStyleArray;


  {This class is a kind of widget class which implements the ISHRenderer
   interface for drawing syntax highlighted text}
  TGtkSHEdit = class(ISHRenderer)
  protected
    SHStyles: PSHStyleArray;
    SHStyleCount: Integer;              // # of currently registered styles
    shWhitespace: Integer;
    CurGCColor: LongWord;

    hadj, vadj: PGtkAdjustment;
    PaintBox: PGtkWidget;
    Edit: TSHTextEdit;
    LeftIndent: Integer;
    CharW, CharH: Integer;
    Font: array[TSHFontStyle] of PGdkFont; // Fonts for content drawing
    gc: PGdkGC;
    GdkWnd: PGdkWindow;

    procedure SetEdit(AEdit: TSHTextEdit);
    procedure SetGCColor(AColor: LongWord);

    // ISHRenderer Implemenation:

    procedure InvalidateLines(y1, y2: Integer); override;

    // Drawing
    procedure ClearRect(x1, y1, x2, y2: Integer); override;
    procedure DrawTextLine(x1, x2, y: Integer; s: PChar); override;

    // Cursor
    procedure ShowCursor(x, y: Integer); override;
    procedure HideCursor(x, y: Integer); override;

    // Scrolling support
    function  GetVertPos: Integer; override;
    procedure SetVertPos(y: Integer); override;
    function  GetPageHeight: Integer; override;
    procedure SetLineCount(count: Integer); override;

    // Clipboard support
    //function  GetClipboard: String; override;
    //procedure SetClipboard(Content: String); override;

  public
    Widget: PGtkWidget;  // this is the outer editor widget

    constructor Create;

    procedure SetFocus;

    function  AddSHStyle(AName: String; AColor, ABackground: LongWord;
      AStyle: TSHFontStyle): Integer;
  end;


implementation

{*****************************************************************************
                              GTK/GDK Callbacks
*****************************************************************************}

procedure TGtkSHEdit_Expose(GtkWidget: PGtkWidget; event: PGdkEventExpose; edit: TGtkSHEdit); cdecl;
var
  x1, y1, x2, y2: Integer;
begin
  x1 := event^.area.x div edit.CharW;
  if x1>0 then
   dec(x1);
  y1 := event^.area.y div edit.CharH;
  if y1>0 then
   dec(y1);
  x2 := (event^.area.x + event^.area.width - 1) div edit.CharW+1;
  y2 := (event^.area.y + event^.area.height - 1) div edit.CharH+1;
  WriteLn(Format('Expose(%d/%d - %d/%d) for %s', [x1, y1, x2, y2, edit.ClassName]));

  edit.GdkWnd := edit.PaintBox^.window;
  edit.GC := gdk_gc_new(edit.GdkWnd);
  gdk_gc_copy(edit.GC, PGtkStyle(edit.PaintBox^.thestyle)^.
    fg_gc[edit.PaintBox^.state]);

  edit.Edit.DrawContent(x1, y1, x2, y2);
end;

function TGtkSHEdit_KeyPressed(GtkWidget: PGtkWidget; Event: PGdkEventKey; edit: TGtkSHEdit): Integer; cdecl;
var
  KeyState,
  KeyCode: LongWord;
  KeyMods: TShiftState;
begin
  Result := 1;

  Case Event^.KeyVal of
    GDK_KP_Insert    : KeyCode:=GDK_Insert;
    GDK_KP_Home      : KeyCode:=GDK_Home;
    GDK_KP_Left      : KeyCode:=GDK_Left;
    GDK_KP_Up        : KeyCode:=GDK_Up;
    GDK_KP_Right     : KeyCode:=GDK_Right;
    GDK_KP_Down      : KeyCode:=GDK_Down;
    GDK_KP_Page_Up   : KeyCode:=GDK_Page_Up;
    GDK_KP_Page_Down : KeyCode:=GDK_Page_Down;
    GDK_KP_End       : KeyCode:=GDK_End;
  else
    KeyCode:=Event^.KeyVal;
  end;

  KeyMods := [];
  KeyState:=Event^.State;
  if (KeyState and 1) <> 0 then KeyMods := KeyMods + [ssShift];
  if (KeyState and 2) <> 0 then KeyMods := KeyMods + [ssCaps];
  if (KeyState and 4) <> 0 then KeyMods := KeyMods + [ssCtrl];
  if (KeyState and 8) <> 0 then KeyMods := KeyMods + [ssAlt];
  if (KeyState and $10) <> 0 then KeyMods := KeyMods + [ssNum];
  if (KeyState and $40) <> 0 then KeyMods := KeyMods + [ssSuper];
  if (KeyState and $80) <> 0 then KeyMods := KeyMods + [ssScroll];
  if (KeyState and $100) <> 0 then KeyMods := KeyMods + [ssLeft];
  if (KeyState and $200) <> 0 then KeyMods := KeyMods + [ssMiddle];
  if (KeyState and $400) <> 0 then KeyMods := KeyMods + [ssRight];
  if (KeyState and $2000) <> 0 then KeyMods := KeyMods + [ssAltGr];

  WriteLn('KeyCode ', KeyCode);

  edit.Edit.KeyPressed(KeyCode,KeyMods);
end;

function TGtkSHEdit_ButtonPressEvent(GtkWidget: PGtkWidget; event: PGdkEventButton ;  edit: TGtkSHEdit): Integer; cdecl;
begin
  Writeln('button press');
  Result := 1;
end;


function TGtkShEdit_FocusInEvent(GtkWidget: PGtkWidget; event: PGdkEventFocus; edit: TGtkSHEdit): Integer; cdecl;
begin
  Writeln('focus in');
  edit.Edit.FocusIn;
  result:=1;
end;


function TGtkShEdit_FocusOutEvent(GtkWidget: PGtkWidget; event: PGdkEventFocus; edit: TGtkSHEdit): Integer; cdecl;
begin
  Writeln('focus out');
  edit.Edit.FocusOut;
  result:=1;
end;


{*****************************************************************************
                                 TGtkSHEdit
*****************************************************************************}

constructor TGtkSHEdit.Create;
var
  lfd: String;    // Logical font descriptor
  i: Integer;
begin
  inherited Create;

  // Create fonts
  for i := 0 to 3 do begin
    lfd := '-*-courier-';
    if (i and 1) <> 0 then lfd := lfd + 'bold'
    else lfd := lfd + 'medium';
    lfd := lfd + '-';
    if (i and 2) <> 0 then lfd := lfd + 'i'
    else lfd := lfd + 'r';
    lfd := lfd + '-normal--14-*-*-*-*-*-iso8859-1';
    Font[TSHFontStyle(i)] := gdk_font_load(PChar(lfd));
  end;

  CharW := gdk_char_width(Font[fsBold], ' ');
  CharH := 14 {=FontHeight} + 3;   // *** find better way to determine max. cell height
  Edit := nil;

  LeftIndent := CharW;

  // Create scrolled window and drawing area

  hadj := PGtkAdjustment(gtk_adjustment_new(0, 0, 200, 1, 10, 100));
  vadj := PGtkAdjustment(gtk_adjustment_new(0, 0, 200, 1, 10, 100));
  Widget := gtk_scrolled_window_new(hadj, vadj);

  PaintBox := gtk_drawing_area_new;

  gtk_scrolled_window_add_with_viewport(PGtkScrolledWindow(Widget), PaintBox);
  gtk_widget_show(PaintBox);

  PGtkObject(PaintBox)^.flags := PGtkObject(PaintBox)^.flags or GTK_CAN_FOCUS;

  gtk_signal_connect_after(PGtkObject(PaintBox), 'expose-event',
    GTK_SIGNAL_FUNC(@TGtkSHEdit_Expose), self);
  gtk_signal_connect_after(PGtkObject(PaintBox), 'key-press-event',
    GTK_SIGNAL_FUNC(@TGtkSHEdit_KeyPressed), self);
  gtk_signal_connect(PGtkObject(PaintBox), 'button-press-event',
    GTK_SIGNAL_FUNC(@TGtkSHEdit_KeyPressed), self);
  gtk_signal_connect(PGtkObject(PaintBox), 'focus-in-event',
    GTK_SIGNAL_FUNC(@TGtkSHEdit_FocusInEvent), self);
  gtk_signal_connect(PGtkObject(PaintBox), 'focus-out-event',
    GTK_SIGNAL_FUNC(@TGtkSHEdit_FocusOutEvent), self);
  gtk_widget_show(Widget);
end;

procedure TGtkSHEdit.SetEdit(AEdit: TSHTextEdit);
begin
  Edit := AEdit;
  shWhitespace      := AddSHStyle('Whitespace', colBlack, colWhite, fsNormal);
  Edit.shDefault    := AddSHStyle('Default',    colBlack, colWhite, fsNormal);
  Edit.shSelected   := AddSHStyle('Selected',   colWhite, colBlack, fsNormal);
{ Install keys }
  Edit.AddKeyDef(@Edit.CursorUp, 'Cursor up', GDK_Up, []);
  Edit.AddKeyDef(@Edit.CursorDown, 'Cursor down', GDK_Down, []);
  Edit.AddKeyDef(@Edit.CursorLeft, 'Cursor left', GDK_Left, []);
  Edit.AddKeyDef(@Edit.CursorRight, 'Cursor right', GDK_Right, []);
  Edit.AddKeyDef(@Edit.CursorHome, 'Cursor Home', GDK_Home, []);
  Edit.AddKeyDef(@Edit.CursorEnd, 'Cursor Home', GDK_End, []);
  Edit.AddKeyDef(@Edit.CursorPageUp, 'Cursor PageUp', GDK_Page_Up, []);
  Edit.AddKeyDef(@Edit.CursorPageDown, 'Cursor PageDown', GDK_Page_Down, []);

  Edit.AddKeyDef(@Edit.ToggleOverwriteMode, 'Toggle overwrite mode', GDK_Insert, []);
  Edit.AddKeyDef(@Edit.EditDelLeft, 'Delete char left of cursor', GDK_Backspace, []);
  Edit.AddKeyDef(@Edit.EditDelRight, 'Delete char right of cursor', GDK_Delete, []);
  Edit.AddKeyDef(@Edit.EditDelLine, 'Delete current line', Ord('Y'), [ssCtrl]);
  Edit.AddKeyDef(@Edit.EditUndo, 'Undo last action', GDK_Backspace, [ssAlt]);
  Edit.AddKeyDef(@Edit.EditRedo, 'Redo last undone action', GDK_Backspace, [ssShift, ssAlt]);
end;


function TGtkSHEdit.AddSHStyle(AName: String; AColor, ABackground: LongWord; AStyle: TSHFontStyle): Integer;
begin
  ReAllocMem(SHStyles, SizeOf(TSHStyle) * (SHStyleCount + 1));
  Inc(SHStyleCount);
  SHStyles^[SHStyleCount].Name       := AName;
  SHStyles^[SHStyleCount].Color      := AColor;
  SHStyles^[SHStyleCount].Background := ABackground;
  SHStyles^[SHStyleCount].FontStyle  := AStyle;
  Result := SHStyleCount;
end;

procedure TGtkSHEdit.SetGCColor(AColor: LongWord);
var
  c: TGdkColor;
begin
  if AColor <> CurGCColor then begin
    c.pixel := 0;
    c.red   := (((AColor shr 16) and 255) * 65535) div 255;
    c.green := (((AColor shr 8) and 255) * 65535) div 255;
    c.blue  := ((AColor and 255) * 65535) div 255;
    gdk_colormap_alloc_color(gdk_colormap_get_system, @c, False, True);
    gdk_gc_set_foreground(gc, @c);
    CurGCColor := AColor;
  end;
end;

procedure TGtkSHEdit.ClearRect(x1, y1, x2, y2: Integer);
begin
  SetGCColor(SHStyles^[shWhitespace].Background);
  gdk_draw_rectangle(PGdkDrawable(GdkWnd), GC, 1,
    x1 * CharW + LeftIndent, y1 * CharH,
    (x2 - x1 + 1) * CharW, (y2 - y1 + 1) * CharH);
end;


procedure TGtkSHEdit.InvalidateLines(y1, y2: Integer);
var
  r : TGdkRectangle;
  w,h : integer;
begin
  gdk_window_get_size(PGdkDrawable(GdkWnd),@w,@h);
  r.x:=0;
  r.y:=y1 * CharH;
  r.Width:=w;
  r.Height:=(y2 - y1 + 1) * CharH;
  gtk_widget_draw(PGtkWidget(PaintBox), @r);
end;


procedure TGtkSHEdit.DrawTextLine(x1, x2, y: Integer; s: PChar);
var
  CurColor: LongWord;
  rx1,rx2 : Integer;

  procedure doerase;
  begin
    if rx2>rx1 then
     begin
       SetGCColor(CurColor);
       gdk_draw_rectangle(PGdkDrawable(GdkWnd), GC, 1,
                          x1 * CharW + LeftIndent, y * CharH, (rx2 - rx1 + 1) * CharW + LeftIndent, CharH);
       rx1:=rx2;
     end;
  end;

var
  RequestedColor: Char;
  i, j, px: Integer;
  NewColor: LongWord;
  hs : pchar;
begin
  {WriteLn(Format('DrawTextLine(%d) for %s ', [y, ClassName]));}

  // Erase the (potentially multi-coloured) background

  rx1 := x1;
  rx2 := 0;
  j := 0;
  CurColor := SHStyles^[shWhitespace].Background;

  // Clear background
  hs:=s;
  rx2:=0;
  repeat
    case hs[0] of
      #0 :
        break;
      LF_Escape :
        begin
          NewColor := SHStyles^[Ord(hs[1])].Background;
          if NewColor = colDefault then
           NewColor := SHStyles^[shWhitespace].Background;
          if (NewColor <> CurColor) then
           begin
             DoErase;
             CurColor := NewColor;
           end;
          Inc(hs, 2);
        end;
      #9 :
        begin
          repeat
            Inc(rx2, CharW);
            Inc(i);
          until (i and 7) = 0;
          Inc(hs);
        end;
      else
        begin
          Inc(hs);
          Inc(i);
          Inc(rx2);
        end;
    end;
  until false;
  rx2 := x2;
  DoErase;

  // Draw text line
  RequestedColor := #1;
  CurGCColor := colInvalid;
  i := 0;
  px := 0;
  repeat
    case s[0] of
      #0 :
        break;
      LF_Escape :
        begin
          RequestedColor := s[1];
          Inc(s, 2);
        end;
      #9 :
        begin
          repeat
            Inc(px, CharW);
            Inc(i);
          until (i and 7) = 0;
          Inc(s);
        end;
      else
        begin
          if (px >= x1) and (px <= x2) then
           begin
             SetGCColor(SHStyles^[Ord(RequestedColor)].Color);
             gdk_draw_text(PGdkDrawable(GdkWnd),
                           Font[SHStyles^[Ord(RequestedColor)].FontStyle], GC,
                           px * CharW + LeftIndent, (y + 1) * CharH - 3, s, 1);
           end;
          Inc(s);
          Inc(i);
          Inc(px);
        end;
    end;
  until false;
{ Also draw the cursor }
{  if y=edit.CursorY then
   DrawCursor; }
end;


procedure TGtkSHEdit.SetFocus;
begin
  gtk_window_set_focus(PGtkWindow(gtk_widget_get_toplevel(Paintbox)),Paintbox);
end;


procedure TGtkSHEdit.ShowCursor(x, y: Integer);
var
  r : TGdkRectangle;
  px,py : integer;
begin
  writeln('Showcursor ',x,',',y);
    px:=x*CharW + LeftIndent;
    py:=y*CharH;
    SetGCColor(colBlack);
    gdk_draw_rectangle(PGdkDrawable(GdkWnd), GC, 1, px, py, px+2, py+CharH);

{  r.x:=x * CharW;
  r.y:=y * CharH;
  r.Width:=CharW;
  r.Height:=CharH;
  gtk_widget_draw(PGtkWidget(PaintBox), @r); }
end;


procedure TGtkSHEdit.HideCursor(x, y: Integer);
var
  r : TGdkRectangle;
begin
  writeln('Hidecursor ',x,',',y);
  r.x:=x * CharW + LeftIndent;
  r.y:=y * CharH;
  r.Width:=CharW;
  r.Height:=CharH;
  gtk_widget_draw(PGtkWidget(PaintBox), @r);
end;


procedure TGtkSHEdit.SetLineCount(count: Integer);
begin
  vadj^.upper := count * CharH;
  gtk_adjustment_changed(vadj);
  gtk_widget_set_usize(PaintBox, Trunc(hadj^.upper), Trunc(vadj^.upper));
end;


function TGtkSHEdit.GetVertPos: Integer;
begin
  Result := Trunc(vadj^.value) div CharH;
end;


procedure TGtkSHEdit.SetVertPos(y: Integer);
begin
  gtk_adjustment_set_value(vadj, y*CharH);
end;


function TGtkSHEdit.GetPageHeight: Integer;
begin
  Result := Trunc(vadj^.page_size) div CharH;
end;

end.
{
  $Log$
  Revision 1.2  1999-12-06 21:27:27  peter
    * gtk updates, redrawing works now much better and clears only between
      x1 and x2

  Revision 1.1  1999/11/15 21:47:36  peter
    * first working keypress things

}