program Test;

uses strings;

var Str1 : PChar;

begin
  GetMem(Str1,256);
  StrPCopy (Str1, ParamStr(0));
  writeln ('Arg 0 is "',Str1,'"');
  StrPCopy (Str1, ParamStr(1));
  writeln ('Arg 1 is "',Str1,'"');
  FreeMem(Str1,256);
end.
