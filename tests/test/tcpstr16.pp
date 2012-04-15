program tcpstr16;
{$mode delphi}
{$codepage cp1251}
{$apptype console}
type
  T866String = type AnsiString(866);
  T1251String = type AnsiString(1251);

function Compare(const S1, S2: RawByteString): Boolean;
begin
  Result :=
    (Length(S1) = Length(S2)) and
    (CompareByte(S1[1],S2[1],Length(S1))=0);
end;

begin
  if Compare(T866String('привет'), 'привет') then
    halt(1);
  if not Compare(AnsiString(T866String('привет')), 'привет') then
    halt(2);
end.
