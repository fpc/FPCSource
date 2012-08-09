{ %NORUN }
program toperator78;

operator ** (left, right: LongInt) res : LongInt;
begin

end;

operator >< (left, right: LongInt) res : LongInt;
begin

end;

operator + (left: LongInt; right: AnsiString) res : AnsiString;
begin

end;

operator - (left, right: AnsiString) res : AnsiString;
begin

end;

operator div (left, right: Single) res : Single;
begin

end;

operator mod (left, right: Double) res : Double;
begin

end;

type
  TTest = (One, Two, Three);
  TTests = set of TTest;

operator and (left, right: TTests) res : TTests;
begin

end;

operator < (left, right: TObject) res : Boolean;
begin

end;

operator + (left: Pointer; right: ShortString) res : ShortString;
begin

end;

operator and (left: array of Char; right: AnsiString) res : AnsiString;
begin

end;

operator + (left: array of Char; right: TTest) res : ShortString;
begin

end;

begin

end.
