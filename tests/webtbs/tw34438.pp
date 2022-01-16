{%norun}
{$mode objfpc}
uses
  types,math;

type
  PGtkWidget = pointer;
  PGtkNotebook = pointer;

function MyRect(Left,Top,Right,Bottom : Integer) : TRect; inline;

begin
  MyRect.Left:=Left;
  MyRect.Top:=Top;
  MyRect.Right:=Right;
  MyRect.Bottom:=Bottom;
end;

function GetWidgetClientRect(TheWidget: PGtkWidget): TRect;
var
  Widget, ClientWidget: PGtkWidget;
  AChild: PGtkWidget;

  procedure GetNoteBookClientRect(NBWidget: PGtkNotebook);
  var
    PageIndex: LongInt;
    PageWidget: PGtkWidget;
    FrameBorders: TRect;
    aWidth: LongInt;
    aHeight: LongInt;
  begin
      Result:=MyRect(0,0,
         Max(0,AWidth-FrameBorders.Left-FrameBorders.Right),
         Max(0,aHeight-FrameBorders.Top-FrameBorders.Bottom));
  end;

begin
end;


begin
end.
