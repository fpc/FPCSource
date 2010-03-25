// test that enumeration members are retrieved from the right enumeration types
// test enumeration.element syntax
// test scropedenums directive
program tenum3;
uses
  typinfo;
type
{$SCOPEDENUMS ON}
  TEnum1 = (first, second, third);
{$SCOPEDENUMS OFF}
  TEnum2 = (zero, first, second, third);
  TRange1 = first..second;
var
  En1: TEnum1;
  En2: TEnum2;
  R1: TRange1;
begin
  En1 := TEnum1.first;
  if GetEnumName(TypeInfo(TEnum1), Ord(En1)) <> 'first' then
    halt(1);
  if Ord(En1) <> 0 then
    halt(2);
  En2 := first;
  if GetEnumName(TypeInfo(TEnum2), Ord(En2)) <> 'first' then
    halt(3);
  if ord(En2) <> 1 then
    halt(4);
  En2 := TEnum2.second;
  if GetEnumName(TypeInfo(TEnum2), Ord(En2)) <> 'second' then
    halt(5);
  if ord(En2) <> 2 then
    halt(6);
  R1 := TRange1.second;
  if GetEnumName(TypeInfo(TRange1), Ord(R1)) <> 'second' then
    halt(7);
  if ord(R1) <> 2 then
    halt(8);
  writeln('ok');
end.
