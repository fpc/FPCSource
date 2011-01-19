{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}
uses
  Classes, SysUtils;

var
  B: Boolean;
  S: String;
begin
  WriteLn('BoolToStr(False, True): ' + BoolToStr(False, True));
  if BoolToStr(False, True)<>'False' then
    halt(1);
  WriteLn('BoolToStr(True, True): ' + BoolToStr(True, True));
  if BoolToStr(True, True)<>'True' then
    halt(2);

  SetLength(TrueBoolStrs, 1);
  SetLength(FalseBoolStrs, 1);
  TrueBoolStrs[0] := 'Sim';
  FalseBoolStrs[0] := 'Não';

  WriteLn('BoolStrs = Não;Sim');

  WriteLn('BoolToStr(False, True): ' + BoolToStr(False, True));
  WriteLn('BoolToStr(True, True): ' + BoolToStr(True, True));

  S := BoolToStr(False, True);
  if S<>'Não' then
    halt(3);
  B := StrToBool(S);
  if b<>false then
    halt(4);

  WriteLn('StrToBool(' + S +') = ' + BoolToStr(B, True));
  if BoolToStr(B, True)<>'Não' then
    halt(5);
  S := BoolToStr(True, True);
  if s<>'Sim' then
    halt(6);
  B := StrToBool(S);
  if b<>true then
    halt(7);
  WriteLn('StrToBool(' + S +') = ' + BoolToStr(B, True));
  if BoolToStr(B, True)<>'Sim' then
    halt(8);

  { should give exception }
  if TryStrToBool('True',B) then
    halt(9);
end.
