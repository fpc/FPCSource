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

    procedure InvalidateRect(x1, y1, x2, y2: Integer); override;
    procedure InvalidateLines(y1, y2: Integer); override;

    // Drawing
    procedure ClearRect(x1, y1, x2, y2: Integer); override;
    procedure DrawTextLine(x1, x2, y: Integer; s: PChar); override;

    // Cursor
    procedure ShowCursor(x, y: Integer); override;
    procedure HideCursor(x, y: Integer); override;

    // Scrolling support
    function  GetHorzPos: Integer; override;
    procedure SetHorzPos(x: Integer); override;
    function  GetVertPos: Integer; override;
    procedure SetVertPos(y: Integer); override;
    function  GetPageWidth: Integer; override;
    function  GetPageHeight: Integer; override;
    function  GetLineWidth: Integer; override;
    procedure SetLineWidth(count: Integer); override;
    function  GetLineCount: Integer; override;
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
  x1 := event^.area.x;
  if x1 > 0 then
    Dec(x1, edit.LeftIndent);
  x2 := x1 + event^.area.width - 1;
  x1 := x1 div edit.CharW;
  x2 := (x2 + edit.CharW - 1) div edit.CharW;
  y1 := event^.area.y div edit.CharH;
  y2 := (event^.area.y + event^.area.height - 1) div edit.CharH;
//  WriteLn(Format('Expose(%d/%d - %d/%d) for %s', [x1, y1, x2, y2, edit.ClassName]));

  edit.GdkWnd := edit.PaintBox^.window;
  edit.GC := gdk_gc_new(edit.GdkWnd);
  edit.CurGCColor := 0;         // Reset color, because we have a new GC!
  gdk_gc_copy(edit.GC, PGtkStyle(edit.PaintBox^.thestyle)^.
    fg_gc[edit.PaintBox^.state]);

  edit.Edit.AdjustCursorToRange;
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
    GDK_Scroll_Lock,
    GDK_Num_Lock,
    GDK_Shift_L..GDK_Hyper_R :
      begin
        // Don't let modifier keys trough as normal keys
        // *** This doesn't work reliably! (sg)
        exit;
      end;
  else
    KeyCode:=Event^.KeyVal;
  end;
  KeyState:=Event^.State;

  WriteLn('KeyCode ', KeyCode,'   keystate ',KeyState);

  // Calculate the Key modifiers (shiftstate)
  KeyMods := [];
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

  edit.Edit.KeyPressed(KeyCode,KeyMods);
end;


function TGtkSHEdit_ButtonPressEvent(GtkWidget: PGtkWidget; event: PGdkEventButton ;  edit: TGtkSHEdit): Integer; cdecl;
var
  px,py : integer;
begin
  px:=(round(event^.x)-edit.leftindent) div edit.CharW;
  py:=(round(event^.y)-edit.leftindent) div edit.CharH;
//  Writeln('button press ',px,',',py);
  edit.Edit.CursorX:=px;
  edit.Edit.CursorY:=py;
  edit.SetFocus;
  Result := 1;
end;


function TGtkShEdit_FocusInEvent(GtkWidget: PGtkWidget; event: PGdkEventFocus; edit: TGtkSHEdit): Integer; cdecl;
begin
//  Writeln('focus in');
  edit.Edit.FocusIn;
  result:=1;
end;


function TGtkShEdit_FocusOutEvent(GtkWidget: PGtkWidget; event: PGdkEventFocus; edit: TGtkSHEdit): Integer; cdecl;
begin
//  Writeln('focus out');
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

  gtk_widget_set_flags(PGtkWidget(PaintBox),GTK_CAN_FOCUS);

  gtk_signal_connect(PGtkObject(PaintBox), 'expose-event',
    GTK_SIGNAL_FUNC(@TGtkSHEdit_Expose), self);
  gtk_signal_connect_after(PGtkObject(PaintBox), 'key-press-event',
    GTK_SIGNAL_FUNC(@TGtkSHEdit_Keypressed), self);
  gtk_signal_connect(PGtkObject(PaintBox), 'button-press-event',
    GTK_SIGNAL_FUNC(@TGtkSHEdit_ButtonPressEvent), self);
  gtk_signal_connect_after(PGtkObject(PaintBox), 'focus-in-event',
    GTK_SIGNAL_FUNC(@TGtkSHEdit_FocusInEvent), self);
  gtk_signal_connect_after(PGtkObject(PaintBox), 'focus-out-event',
    GTK_SIGNAL_FUNC(@TGtkSHEdit_FocusOutEvent), self);

  gtk_widget_set_events(PGtkWidget(Paintbox),
    GDK_EXPOSURE_MASK or GDK_KEY_PRESS_MASK or GDK_KEY_RELEASE_MASK or
    GDK_BUTTON_PRESS_MASK or GDK_ENTER_NOTIFY_MASK or GDK_LEAVE_NOTIFY_MASK);

  gtk_widget_show(Widget);
end;


procedure TGtkSHEdit.SetEdit(AEdit: TSHTextEdit);
begin
  Edit := AEdit;
  shWhitespace      := AddSHStyle('Whitespace', colBlack, colWhite,    fsNormal);
  Edit.shDefault    := AddSHStyle('Default',    colBlack, colWhite,    fsNormal);
  Edit.shSelected   := AddSHStyle('Selected',   colWhite, colDarkBlue, fsNormal);
{ Install keys }
  Edit.AddKeyDef(@Edit.CursorUp, selClear, 'Cursor up', GDK_Up, []);
  Edit.AddKeyDef(@Edit.CursorDown, selClear, 'Cursor down', GDK_Down, []);
  Edit.AddKeyDef(@Edit.CursorLeft, selClear, 'Cursor left', GDK_Left, []);
  Edit.AddKeyDef(@Edit.CursorRight, selClear, 'Cursor right', GDK_Right, []);
  Edit.AddKeyDef(@Edit.CursorHome, selClear, 'Cursor Home', GDK_Home, []);
  Edit.AddKeyDef(@Edit.CursorEnd, selClear, 'Cursor Home', GDK_End, []);
  Edit.AddKeyDef(@Edit.CursorPageUp, selClear, 'Cursor PageUp', GDK_Page_Up, []);
  Edit.AddKeyDef(@Edit.CursorPageDown, selClear, 'Cursor PageDown', GDK_Page_Down, []);
  Edit.AddKeyDef(@Edit.CursorDocBegin, selClear, 'Cursor Document Start', GDK_Page_Up, [ssCtrl]);
  Edit.AddKeyDef(@Edit.CursorDocEnd, selClear, 'Cursor Document End', GDK_Page_Down, [ssCtrl]);

  Edit.AddKeyDef(@Edit.CursorUp, selExtend, 'Selection up', GDK_Up, [ssShift]);
  Edit.AddKeyDef(@Edit.CursorDown, selExtend, 'Selection down', GDK_Down, [ssShift]);
  Edit.AddKeyDef(@Edit.CursorLeft, selExtend, 'Selection left', GDK_Left, [ssShift]);
  Edit.AddKeyDef(@Edit.CursorRight, selExtend, 'Selection right', GDK_Right, [ssShift]);
  Edit.AddKeyDef(@Edit.CursorHome, selExtend, 'Selection Home', GDK_Home, [ssShift]);
  Edit.AddKeyDef(@Edit.CursorEnd, selExtend, 'Selection Home', GDK_End, [ssShift]);
  Edit.AddKeyDef(@Edit.CursorPageUp, selExtend, 'Selection PageUp', GDK_Page_Up, [ssShift]);
  Edit.AddKeyDef(@Edit.CursorPageDown, selExtend, 'Selection PageDown', GDK_Page_Down, [ssShift]);
  Edit.AddKeyDef(@Edit.CursorDocBegin, selExtend, 'Selection Document Start', GDK_Page_Up, [ssCtrl,ssShift]);
  Edit.AddKeyDef(@Edit.CursorDocEnd, selExtend, 'Selection Document End', GDK_Page_Down, [ssCtrl,ssShift]);

  Edit.AddKeyDef(@Edit.ToggleOverwriteMode, selNothing, 'Toggle overwrite mode', GDK_Insert, []);
  Edit.AddKeyDef(@Edit.EditDelLeft, selClear, 'Delete char left of cursor', GDK_Backspace, []);
  Edit.AddKeyDef(@Edit.EditDelRight, selClear, 'Delete char right of cursor', GDK_Delete_Key, []);
  Edit.AddKeyDef(@Edit.EditDelLine, selClear, 'Delete current line', Ord('Y'), [ssCtrl]);
  Edit.AddKeyDef(@Edit.EditDelLine, selClear, 'Delete current line', Ord('y'), [ssCtrl]);
  Edit.AddKeyDef(@Edit.EditUndo, selClear, 'Undo last action', GDK_Backspace, [ssAlt]);
  Edit.AddKeyDef(@Edit.EditRedo, selClear, 'Redo last undone action', GDK_Backspace, [ssShift, ssAlt]);
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


procedure TGtkSHEdit.InvalidateRect(x1, y1, x2, y2: Integer);
var
  r : TGdkRectangle;
begin
  r.x := x1 * CharW + LeftIndent;
  r.y := y1 * CharH;
  r.Width := (x2 - x1 + 1) * CharW;
  r.Height := (y2 - y1 + 1) * CharH;
  gtk_widget_draw(PGtkWidget(PaintBox), @r);
end;


procedure TGtkSHEdit.InvalidateLines(y1, y2: Integer);
var
  r : TGdkRectangle;
  w,h : integer;
begin
  gdk_window_get_size(PGdkDrawable(GdkWnd), @w, @h);
  r.x := 0;
  r.y := y1 * CharH;
  r.Width := w;
  r.Height := (y2 - y1 + 1) * CharH;
  gtk_widget_draw(PGtkWidget(PaintBox), @r);
end;


procedure TGtkSHEdit.DrawTextLine(x1, x2, y: Integer; s: PChar);
var
  CurColor: LongWord;
  CurX1, CurX2: Integer;

  procedure DoErase;
  begin
    SetGCColor(CurColor);
    if CurX1 < x1 then
      CurX1 := x1;
    if CurX2 > CurX1 then begin
      gdk_draw_rectangle(PGdkDrawable(GdkWnd), GC, 1,
        CurX1 * CharW + LeftIndent, y * CharH, (CurX2 - CurX1) * CharW, CharH);
    end;
    CurX1 := CurX2;
  end;

var
  RequestedColor: Integer;
  NewColor: LongWord;
  hs : PChar;
begin

  // Erase the (potentially multi-coloured) background

  hs := s;
  CurColor := SHStyles^[shWhitespace].Background;

  CurX1 := 0;
  CurX2 := 0;
  while (hs[0] <> #0) and (CurX2 <= x2) do begin
    case hs[0] of
      LF_Escape: begin
          NewColor := SHStyles^[Ord(hs[1])].Background;
          if NewColor = colDefault then
            NewColor := SHStyles^[shWhitespace].Background;
          if NewColor <> CurColor then begin
            DoErase;
            CurColor := NewColor;
          end;
          Inc(hs, 2);
        end;
      #9: begin
          repeat
            Inc(CurX2);
          until (CurX2 and 7) = 0;
          Inc(hs);
        end;
      else begin
        Inc(hs);
        Inc(CurX2);
      end;
    end;
  end;
  CurX2 := x2;
  DoErase;


  // Draw text line

  RequestedColor := shWhitespace;
  CurX1 := 0;
  while s[0] <> #0 do
    case s[0] of
      LF_Escape: begin
          RequestedColor := Ord(s[1]);
          Inc(s, 2);
        end;
      #9: begin
          repeat
            Inc(CurX1);
          until (CurX1 and 7) = 0;
          Inc(s);
        end;
      ' ': begin
          Inc(s);
          Inc(CurX1);
        end;
      else begin
        if (CurX1 >= x1) and (CurX1 <= x2) then begin
          SetGCColor(SHStyles^[RequestedColor].Color);
          gdk_draw_text(PGdkDrawable(GdkWnd),
            Font[SHStyles^[RequestedColor].FontStyle], GC,
            CurX1 * CharW + LeftIndent, (y + 1) * CharH - 3, s, 1);
        end;
        Inc(s);
        Inc(CurX1);
      end;
    end;
end;


procedure TGtkSHEdit.SetFocus;
begin
  gtk_window_set_focus(PGtkWindow(gtk_widget_get_toplevel(Paintbox)),Paintbox);
end;


procedure TGtkSHEdit.ShowCursor(x, y: Integer);
begin
//  writeln('Showcursor ',x,',',y);
  if assigned(GdkWnd) then
   begin
     SetGCColor(colBlack);
     gdk_draw_rectangle(PGdkDrawable(GdkWnd), GC, 1, x*CharW + LeftIndent, y*CharH, 2, CharH);
   end;
end;


procedure TGtkSHEdit.HideCursor(x, y: Integer);
var
  r : TGdkRectangle;
begin
//  writeln('Hidecursor ',x,',',y);
  r.x := x * CharW + LeftIndent;
  r.y := y * CharH;
  r.Width := 2;
  r.Height := CharH;
  gtk_widget_draw(PGtkWidget(PaintBox), @r);
end;


function TGtkSHEdit.GetLineWidth: Integer;
begin
  Result := (Trunc(hadj^.upper)-LeftIndent) div CharW;
end;


procedure TGtkSHEdit.SetLineWidth(count: Integer);
begin
  hadj^.upper := count * CharW + LeftIndent;
  gtk_adjustment_changed(hadj);
  gtk_widget_set_usize(PaintBox, Trunc(hadj^.upper), Trunc(vadj^.upper));
end;


function TGtkSHEdit.GetLineCount: Integer;
begin
  Result := Trunc(vadj^.upper) div CharH;
end;


procedure TGtkSHEdit.SetLineCount(count: Integer);
begin
  vadj^.upper := (count+1) * CharH;
  gtk_adjustment_changed(vadj);
  gtk_widget_set_usize(PaintBox, Trunc(hadj^.upper), Trunc(vadj^.upper));
end;


function TGtkSHEdit.GetHorzPos: Integer;
begin
  Result := Trunc(hadj^.value);
  if Result>0 then
   Result:=(Result-LeftIndent) div CharW;
end;


procedure TGtkSHEdit.SetHorzPos(x: Integer);
begin
  if x>0 then
   x:=x*CharW+LeftIndent;
  gtk_adjustment_set_value(hadj, x);
end;


function TGtkSHEdit.GetVertPos: Integer;
begin
  Result := (Trunc(vadj^.value)+CharH-1) div CharH;
end;


procedure TGtkSHEdit.SetVertPos(y: Integer);
begin
  gtk_adjustment_set_value(vadj, y*CharH);
end;


function TGtkSHEdit.GetPageWidth: Integer;
begin
  Result := Trunc(hadj^.page_size) div CharW;
end;


function TGtkSHEdit.GetPageHeight: Integer;
begin
  Result := Trunc(vadj^.page_size) div CharH;
end;

end.
{
  $Log$
  Revision 1.8  1999-12-22 22:28:08  peter
    * updates for cursor setting
    * button press event works

  Revision 1.7  1999/12/12 17:50:50  sg
  * Fixed drawing of selection
  * Several small corrections (removed superfluous local variables etc.)

  Revision 1.6  1999/12/10 15:01:02  peter
    * first things for selection
    * Better Adjusting of range/cursor

  Revision 1.5  1999/12/09 23:16:41  peter
    * cursor walking is now possible, both horz and vert ranges are now
      adapted
    * filter key modifiers
    * selection move routines added, but still no correct output to the
      screen

  Revision 1.4  1999/12/08 01:03:15  peter
    * changes so redrawing and walking with the cursor finally works
      correct

  Revision 1.3  1999/12/08 00:42:54  sg
  * The cursor should be displayed correctly now

  Revision 1.2  1999/12/06 21:27:27  peter
    * gtk updates, redrawing works now much better and clears only between
      x1 and x2

  Revision 1.1  1999/11/15 21:47:36  peter
    * first working keypress things

}