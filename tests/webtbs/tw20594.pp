{ %norun}
{ %OPT=-Sew -vw}
{$MODE delphi}

type
  TTestRec1 = record
    A, B: Integer;
  end;

  TTestRec2 = record
    A, B: Integer;
    class operator Explicit(const rec: TTestRec2): ShortString;
  end;

  TTestRec3 = record
    A, B: Integer;
    function ToString: ShortString;
  end;

  TTestRec4 = record
    A: Integer;
    function ToString: ShortString;
    var B: Integer;
  end;

class operator TTestRec2.Explicit(const rec: TTestRec2): ShortString;
begin
  with rec do WriteStr(Result, A, ':', B);
end;

function TTestRec3.ToString: ShortString;
begin
  Result := ShortString(TTestRec2(Self));
end;

function TTestRec4.ToString: ShortString;
begin
  Result := ShortString(TTestRec2(Self));
end;

const
  r1: TTestRec1 = (A: 1; B: 2);
  r2: TTestRec2 = (A: 3; B: 4);
  r3: TTestRec3 = (A: 5; B: 6);
  r4: TTestRec3 = (A: 7; B: 8);

begin
  Writeln(ShortString(r2));

  Writeln(SizeOf(TTestRec1) = SizeOf(TTestRec2));
  Writeln(ShortString(TTestRec2(r1)));

  Writeln(r3.ToString);
  Writeln(r4.ToString);
end.
