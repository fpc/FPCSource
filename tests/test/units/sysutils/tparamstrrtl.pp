{ ParamStr has the width of a Unicode mode's string: through unit uuchar with an Ansi RTL, and in a Unicode RTL
  (the unicodertl subtarget) by itself. An ansistring there would make ChangeFileExt(ParamStr(0), ...) ambiguous
  between its RawByteString and UnicodeString forms. }
program tparamstrrtl;

{$mode delphiunicode}

uses
  SysUtils;

begin
  if SizeOf(ParamStr(0)[1]) <> 2 then
    Halt(1);
  if ChangeFileExt(ParamStr(0), '.map') = '' then
    Halt(2);
  if ExtractFileExt(ChangeFileExt(ParamStr(0), '.map')) <> '.map' then
    Halt(3);
  WriteLn('ok');
end.
