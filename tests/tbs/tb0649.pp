{ %NORUN }

program tb0649;

{$mode objfpc}

type
  TEnum = (
    eOne,
    eTwo,
    eThree
  );

  TEnumSet = set of TEnum;

  TByteSet = set of Byte;

  TTest = class
  end;

operator + (aLeft: TTest; aRight: array of Byte): TTest;
begin
  Writeln('Array of Byte');
  Result := aLeft;
end;

operator + (aLeft: TTest; aRight: TByteSet): TTest;
begin
  Writeln('Set of Byte');
  Result := aLeft;
end;

operator + (aLeft: TTest; aRight: array of TEnum): TTest;
begin
  Writeln('Array of TEnum');
  Result := aLeft;
end;

operator + (aLeft: TTest; aRight: TEnumSet): TTest;
begin
  Writeln('Set of TEnum');
  Result := aLeft;
end;

var
  t: TTest;
begin
  t := t + [1, 2, 3];
  t := t + [eOne, eTwo];
end.
