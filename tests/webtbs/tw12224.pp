program test_widestrprop_p2;
{$mode objfpc}{$H+}
uses
  Classes, SysUtils, TypInfo;

type
  TClass_A = class(TPersistent)
  private
    Fwsp: UnicodeString;
  published
    property wsp : UnicodeString read Fwsp write Fwsp;
  end;

var
  x : TClass_A;
begin
  x := TClass_A.Create();
  WriteLn('Reading :');
  x.wsp := 'azerty';
  WriteLn(' Using GetUnicodeStrProp() : ',GetUnicodeStrProp(x,'wsp'));
  if GetUnicodeStrProp(x,'wsp')<>'azerty' then
    halt(1);
  WriteLn(' Using GetStrProp() : ',GetStrProp(x,'wsp'));
  if GetStrProp(x,'wsp')<>'azerty' then
    halt(1);
  WriteLn(' Using GetWideStrProp() : ',GetWideStrProp(x,'wsp'));
  if GetWideStrProp(x,'wsp')<>'azerty' then
    halt(1);

  WriteLn('Writing :');
  x.wsp := '';
  SetUnicodeStrProp(x,'wsp','azerty');
  WriteLn(' Using SetUnicodeStrPr() : ',x.wsp);
  if x.wsp<>'azerty' then
    halt(1);
  x.wsp := '';
  SetStrProp(x,'wsp','azerty');
  WriteLn(' Using SetStrPr() : ',x.wsp);
  if x.wsp<>'azerty' then
    halt(1);
  x.wsp := '';
  SetWideStrProp(x,'wsp','azerty');
  WriteLn(' Using SetWideStrPr() : ',x.wsp);
  if x.wsp<>'azerty' then
    halt(1);
  writeln('ok');
end.
