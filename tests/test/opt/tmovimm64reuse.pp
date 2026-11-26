{ %CPU=x86_64 }
{ %OPT=-O3 }
program tmovimm64reuse;
{$mode objfpc}{$H+}
uses Types;

procedure CheckPoints(AFirst, ASecond: TPoint; const AText: string); noinline;
begin
  if (AFirst.Y <> ASecond.Y) or (ASecond.X <> AFirst.X + 2) then
    Halt(1);
end;

function Position(AIndex: Integer): Integer; noinline;
begin
  Result := AIndex * 2 + 7;
end;

procedure Run(AFirst, ALast: Integer); noinline;
var
  LIndex: Integer;
  LPosition: Integer;
begin
  for LIndex := ALast downto AFirst do
  begin
    LPosition := Position(LIndex);
    if LPosition < 1 then
      Continue;
    CheckPoints(Point(LPosition, LIndex), Point(LPosition + 2, LIndex), '');
  end;
end;

begin
  Run(350, 355);
end.
