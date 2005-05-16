{
    $Id: gtkshedit.pp,v 1.5 2005/02/14 17:13:17 peter Exp $

    GTK implementation for SHEdit
    Copyright (C) 1999-2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit GtkSHEdit;
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

  TSHStyleArray = array[1..255] of TSHStyle;  // Notice the 1!
  PSHStyleArray = ^TSHStyleArray;


  {This class is a kind of widget class which implements the ISHWidget
   interface for drawing syntax highlighted text}
  TGtkSHWidget = class(ISHWidget)
  protected
    SHStyles: PSHStyleArray;
    SHStyleCount: Integer;              // # of currently registered styles
    shWhitespace: Integer;
    CurGCColor: LongWord;

    hadj, vadj: PGtkAdjustment;
    PaintBox: PGtkWidget;
    FEdit: TSHTextEdit;
    LeftIndent: Integer;
    CharW, CharH: Integer;
    Font: array[TSHFontStyle] of PGdkFont; // Fonts for content drawing
    gc: PGdkGC;
    GdkWnd: PGdkWindow;

    procedure SetGCColor(AColor: LongWord);

    // ISHWidget Implemenation:

    procedure InvalidateRect(x, y, w, h: Integer); override;

    // Drawing
    procedure ClearRect(x, y, w, h: Integer); override;
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
    function  GetClipboard: String; override;
    procedure SetClipboard(Content: String); override;

  public
    Widget: PGtkWidget;  // this is the outer editor widget

    constructor Create(ADoc: TTextDoc; AEditClass: TSHTextEditClass);
    destructor Destroy; override;

    procedure SetFocus;

    function  AddSHStyle(AName: String; AColor, ABackground: LongWord;
      AStyle: TSHFontStyle): Integer;
    property Edit: TSHTextEdit read FEdit;
  end;


implementation

var
  InternalClipboardContent: String;

{*****************************************************************************
                              GTK/GDK Callbacks
*****************************************************************************}

procedure TGtkSHWidget_Expose(GtkWidget: PGtkWidget; event: PGdkEventExpose;
  widget: TGtkSHWidget); cdecl;
var
  x, y, w, h: Integer;
begin
  x := (event^.area.x - widget.LeftIndent) div widget.CharW;
  y := event^.area.y div widget.CharH;
  w := (event^.area.x + event^.area.width + widget.CharW - 1) div widget.CharW - x;
  h := (event^.area.y + event^.area.height + widget.CharH - 1) div widget.CharH - y;
//  WriteLn(Format('Expose(%d/%d, %dx%d) for %s', [x, y, w, h, FEdit.ClassName]));

  widget.GdkWnd := widget.PaintBox^.window;
  widget.GC := gdk_gc_new(widget.GdkWnd);
  widget.CurGCColor := 0;         // Reset color, because we have a new GC!
  gdk_gc_copy(widget.GC, PGtkStyle(widget.PaintBox^.thestyle)^.
    fg_gc[widget.PaintBox^.state]);

  widget.FEdit.AdjustCursorToRange;
  widget.FEdit.DrawContent(x, y, w, h);
end;


function TGtkSHWidget_KeyPressed(GtkWidget: PGtkWidget; Event: PGdkEventKey;
  widget: TGtkSHWidget): Integer; cdecl;
var
  KeyState,
  KeyCode: LongWord;
  KeyMods: TShiftState;
begin
  Result := 1;

  case Event^.KeyVal of
    GDK_Return       : KeyCode:=13;
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

  // WriteLn('KeyCode ', KeyCode,'   keystate ',KeyState);

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

  widget.FEdit.KeyPressed(KeyCode,KeyMods);
end;


function TGtkSHWidget_ButtonPressEvent(GtkWidget: PGtkWidget;
  event: PGdkEventButton; widget: TGtkSHWidget): Integer; cdecl;
begin
  widget.FEdit.CursorX := Round((event^.x - widget.LeftIndent) / widget.CharW);
  widget.FEdit.CursorY := Trunc(event^.y) div widget.CharH;
  widget.SetFocus;
  Result := 1;
end;


function TGtkSHWidget_FocusInEvent(GtkWidget: PGtkWidget;
  event: PGdkEventFocus; widget: TGtkSHWidget): Integer; cdecl;
begin
//  Writeln('focus in');
  widget.FEdit.FocusIn;
  result:=1;
end;


function TGtkSHWidget_FocusOutEvent(GtkWidget: PGtkWidget; event: PGdkEventFocus; widget: TGtkSHWidget): Integer; cdecl;
begin
//  Writeln('focus out');
  widget.FEdit.FocusOut;
  result:=1;
end;


{*****************************************************************************
                                 TGtkSHWidget
*****************************************************************************}

constructor TGtkSHWidget.Create(ADoc: TTextDoc; AEditClass: TSHTextEditClass);
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
    GTK_SIGNAL_FUNC(@TGtkSHWidget_Expose), self);
  gtk_signal_connect_after(PGtkObject(PaintBox), 'key-press-event',
    GTK_SIGNAL_FUNC(@TGtkSHWidget_Keypressed), self);
  gtk_signal_connect(PGtkObject(PaintBox), 'button-press-event',
    GTK_SIGNAL_FUNC(@TGtkSHWidget_ButtonPressEvent), self);
  gtk_signal_connect_after(PGtkObject(PaintBox), 'focus-in-event',
    GTK_SIGNAL_FUNC(@TGtkSHWidget_FocusInEvent), self);
  gtk_signal_connect_after(PGtkObject(PaintBox), 'focus-out-event',
    GTK_SIGNAL_FUNC(@TGtkSHWidget_FocusOutEvent), self);

  gtk_widget_set_events(PGtkWidget(Paintbox),
    GDK_EXPOSURE_MASK or GDK_KEY_PRESS_MASK or GDK_KEY_RELEASE_MASK or
    GDK_BUTTON_PRESS_MASK or GDK_ENTER_NOTIFY_MASK or GDK_LEAVE_NOTIFY_MASK);

  gtk_widget_show(Widget);


  FEdit := AEditClass.Create(ADoc, Self);
  shWhitespace      := AddSHStyle('Whitespace', colBlack, colWhite,    fsNormal);
  FEdit.shDefault    := AddSHStyle('Default',    colBlack, colWhite,    fsNormal);
  FEdit.shSelected   := AddSHStyle('Selected',   colWhite, colDarkBlue, fsNormal);
{ Install keys }
  FEdit.AddKeyDef(@FEdit.CursorUp, selClear, 'Cursor up', GDK_Up, []);
  FEdit.AddKeyDef(@FEdit.CursorDown, selClear, 'Cursor down', GDK_Down, []);
  FEdit.AddKeyDef(@FEdit.CursorLeft, selClear, 'Cursor left', GDK_Left, []);
  FEdit.AddKeyDef(@FEdit.CursorRight, selClear, 'Cursor right', GDK_Right, []);
  FEdit.AddKeyDef(@FEdit.CursorHome, selClear, 'Cursor Home', GDK_Home, []);
  FEdit.AddKeyDef(@FEdit.CursorEnd, selClear, 'Cursor Home', GDK_End, []);
  FEdit.AddKeyDef(@FEdit.CursorPageUp, selClear, 'Cursor PageUp', GDK_Page_Up, []);
  FEdit.AddKeyDef(@FEdit.CursorPageDown, selClear, 'Cursor PageDown', GDK_Page_Down, []);
  FEdit.AddKeyDef(@FEdit.CursorDocBegin, selClear, 'Cursor Document Start', GDK_Page_Up, [ssCtrl]);
  FEdit.AddKeyDef(@FEdit.CursorDocEnd, selClear, 'Cursor Document End', GDK_Page_Down, [ssCtrl]);

  FEdit.AddKeyDef(@FEdit.CursorUp, selExtend, 'Selection up', GDK_Up, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorDown, selExtend, 'Selection down', GDK_Down, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorLeft, selExtend, 'Selection left', GDK_Left, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorRight, selExtend, 'Selection right', GDK_Right, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorHome, selExtend, 'Selection Home', GDK_Home, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorEnd, selExtend, 'Selection Home', GDK_End, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorPageUp, selExtend, 'Selection PageUp', GDK_Page_Up, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorPageDown, selExtend, 'Selection PageDown', GDK_Page_Down, [ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorDocBegin, selExtend, 'Selection Document Start', GDK_Page_Up, [ssCtrl,ssShift]);
  FEdit.AddKeyDef(@FEdit.CursorDocEnd, selExtend, 'Selection Document End', GDK_Page_Down, [ssCtrl,ssShift]);

  FEdit.AddKeyDef(@FEdit.ToggleOverwriteMode, selNothing, 'Toggle overwrite mode', GDK_Insert, []);
  FEdit.AddKeyDef(@FEdit.EditDelLeft, selClear, 'Delete char left of cursor', GDK_Backspace, []);
  FEdit.AddKeyDef(@FEdit.EditDelRight, selClear, 'Delete char right of cursor', GDK_Delete_Key, []);
  FEdit.AddKeyDef(@FEdit.EditDelLine, selClear, 'Delete current line', Ord('Y'), [ssCtrl]);
  FEdit.AddKeyDef(@FEdit.EditDelLine, selClear, 'Delete current line', Ord('y'), [ssCtrl]);
  FEdit.AddKeyDef(@FEdit.EditUndo, selClear, 'Undo last action', GDK_Backspace, [ssAlt]);
  FEdit.AddKeyDef(@FEdit.EditRedo, selClear, 'Redo last undone action', GDK_Backspace, [ssShift, ssAlt]);
end;

destructor TGtkSHWidget.Destroy;
begin
  FreeMem(SHStyles);
  FEdit.Free;
  inherited Destroy;
end;


function TGtkSHWidget.AddSHStyle(AName: String; AColor, ABackground: LongWord; AStyle: TSHFontStyle): Integer;
begin
  ReAllocMem(SHStyles, SizeOf(TSHStyle) * (SHStyleCount + 1));
  Inc(SHStyleCount);
  SHStyles^[SHStyleCount].Name       := AName;
  SHStyles^[SHStyleCount].Color      := AColor;
  SHStyles^[SHStyleCount].Background := ABackground;
  SHStyles^[SHStyleCount].FontStyle  := AStyle;
  Result := SHStyleCount;
end;


procedure TGtkSHWidget.SetGCColor(AColor: LongWord);
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


procedure TGtkSHWidget.ClearRect(x, y, w, h: Integer);
begin
  SetGCColor(SHStyles^[shWhitespace].Background);
  gdk_draw_rectangle(PGdkDrawable(GdkWnd), GC, 1,
    x * CharW + LeftIndent, y * CharH, w * CharW, h * CharH);
end;


procedure TGtkSHWidget.InvalidateRect(x, y, w, h: Integer);
var
  r : TGdkRectangle;
begin
  r.x := x * CharW + LeftIndent;
  r.y := y * CharH;
  r.Width := w * CharW;
  r.Height := h * CharH;
  gtk_widget_draw(PGtkWidget(PaintBox), @r);
end;


procedure TGtkSHWidget.DrawTextLine(x1, x2, y: Integer; s: PChar);
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


procedure TGtkSHWidget.SetFocus;
begin
  gtk_window_set_focus(PGtkWindow(gtk_widget_get_toplevel(Paintbox)),Paintbox);
end;


procedure TGtkSHWidget.ShowCursor(x, y: Integer);
begin
//  writeln('Showcursor ',x,',',y);
  if assigned(GdkWnd) then
   begin
     SetGCColor(colBlack);
     gdk_draw_rectangle(PGdkDrawable(GdkWnd), GC, 1, x*CharW + LeftIndent, y*CharH, 2, CharH);
   end;
end;


procedure TGtkSHWidget.HideCursor(x, y: Integer);
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


function TGtkSHWidget.GetLineWidth: Integer;
begin
  Result := (Trunc(hadj^.upper)-LeftIndent) div CharW;
end;


procedure TGtkSHWidget.SetLineWidth(count: Integer);
begin
  hadj^.upper := count * CharW + LeftIndent;
  gtk_adjustment_changed(hadj);
  gtk_widget_set_usize(PaintBox, Trunc(hadj^.upper), Trunc(vadj^.upper));
end;


function TGtkSHWidget.GetLineCount: Integer;
begin
  Result := Trunc(vadj^.upper) div CharH;
end;


procedure TGtkSHWidget.SetLineCount(count: Integer);
begin
  vadj^.upper := (count+1) * CharH;
  gtk_adjustment_changed(vadj);
  gtk_widget_set_usize(PaintBox, Trunc(hadj^.upper), Trunc(vadj^.upper));
end;


function TGtkSHWidget.GetClipboard: String;
begin
  Result := InternalClipboardContent;
end;


procedure TGtkSHWidget.SetClipboard(Content: String);
begin
  InternalClipboardContent := Content;
end;


function TGtkSHWidget.GetHorzPos: Integer;
begin
  Result := Trunc(hadj^.value);
  if Result>0 then
   Result:=(Result-LeftIndent) div CharW;
end;


procedure TGtkSHWidget.SetHorzPos(x: Integer);
begin
  if x>0 then
   x:=x*CharW+LeftIndent;
  gtk_adjustment_set_value(hadj, x);
end;


function TGtkSHWidget.GetVertPos: Integer;
begin
  Result := (Trunc(vadj^.value)+CharH-1) div CharH;
end;


procedure TGtkSHWidget.SetVertPos(y: Integer);
begin
  gtk_adjustment_set_value(vadj, y*CharH);
end;


function TGtkSHWidget.GetPageWidth: Integer;
begin
  Result := Trunc(hadj^.page_size) div CharW;
end;


function TGtkSHWidget.GetPageHeight: Integer;
begin
  Result := Trunc(vadj^.page_size) div CharH;
end;

end.

{
  $Log: gtkshedit.pp,v $
  Revision 1.5  2005/02/14 17:13:17  peter
    * truncate log

}
