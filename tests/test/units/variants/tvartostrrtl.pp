{ %skiptarget=qnx,wdosx,palmos,macosclassic,embedded,zxspectrum,msxdos,amstradcpc,ps1 }
{ %skipcpu=jvm }
{ VarToStr and VarToStrDef return the RTL's string: in a Unicode RTL (the unicodertl subtarget) their result
  is a UnicodeString, so a character the system code page cannot hold survives them, in the result and in the
  default, whatever that code page is. }
program tvartostrrtl;

{$mode delphiunicode}

uses
  SysUtils, Variants;

var
  lV: Variant;
  lS: string;
{$ifdef FPC_SUBTARGET_UNICODERTL}
  lW: UnicodeString;
{$endif}

begin
  lV := 5;
  if VarToStr(lV) <> '5' then
    Halt(1);
  if VarToStrDef(Null, 'd') <> 'd' then
    Halt(2);
  if VarToStrDef(lV, 'd') <> '5' then
    Halt(3);
{$ifdef FPC_SUBTARGET_UNICODERTL}
  { the result's own width, which a system code page holding the character would not show }
  if SizeOf(VarToStr(lV)[1]) <> SizeOf(UnicodeChar) then
    Halt(7);
  if SizeOf(VarToStrDef(lV, 'd')[1]) <> SizeOf(UnicodeChar) then
    Halt(8);
  lW := UnicodeChar($0101);
  lV := lW;
  lS := VarToStr(lV);
  if (Length(lS) <> 1) or (lS[1] <> UnicodeChar($0101)) then
    Halt(4);
  lS := VarToStrDef(lV, 'd');
  if (Length(lS) <> 1) or (lS[1] <> UnicodeChar($0101)) then
    Halt(5);
  lS := VarToStrDef(Null, lW);
  if (Length(lS) <> 1) or (lS[1] <> UnicodeChar($0101)) then
    Halt(6);
{$endif}
  lS := '';
  WriteLn('ok', lS);
end.
