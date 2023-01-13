unit VidUtil;

{$MODE objfpc}{$H+}

interface

uses
  KbdUtil;

procedure TextOut(X, Y: Integer; const S: AnsiString; TextAttr: Byte);
procedure DrawKey(const Key: TKey; TextAttr: Byte);
procedure DrawKeyboard(const Kbd: TKeyboard);

implementation

uses
  Video;

procedure TextOut(X, Y: Integer; const S: AnsiString; TextAttr: Byte);
var
  W, P, I, M: Integer;
begin
  P := ((X-1)+(Y-1)*ScreenWidth);
  M := Length(S);
  if (P+M) > ScreenWidth*ScreenHeight then
    M := ScreenWidth*ScreenHeight-P;
  for I := 1 to M do
    VideoBuf^[P+I-1] := Ord(S[I]) + (TextAttr shl 8);
end;

procedure DrawKey(const Key: TKey; TextAttr: Byte);
var
  Y: Integer;
begin
  for Y := Key.YTop to Key.YBottom do
  begin
    if Y = Key.Y then
      TextOut(Key.X + 1, Y + 1, Key.KeyLabel, TextAttr)
    else
      TextOut(Key.X + 1, Y + 1, StringOfChar(' ', Length(Key.KeyLabel)), TextAttr);
  end;
end;

procedure DrawKeyboard(const Kbd: TKeyboard);
var
  I: Integer;
begin
  for I := Low(kbd.Keys) to High(kbd.Keys) do
    DrawKey(kbd.Keys[I], $70);
end;

end.
