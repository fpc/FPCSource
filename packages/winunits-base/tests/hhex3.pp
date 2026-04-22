Program Hhex3;
{
   Small example/test of the html help OCX.
   Marco van de Voort (C) 2026

   Note: use a third party CHM file with context numbers
}
{$mode objfpc}{$H+}
uses htmlhelp,sysutils;


var
   HelpfileName : AnsiString;
   res 		: Integer;
   id           : integer;

Begin
  Writeln('Html example 3');
  Writeln('note: use a third party CHM file with context numbers');
  if paramcount<>2 then
    begin
      writeln('usage:  hhex <chmfile>  <contextnr>');
      halt;
    end;

  Helpfilename:=paramstr(1);

  id:=strtointdef(paramstr(2),1000);
  Writeln('calling context help for ID ',id);
  Res:=HtmlHelpA(0,PAnsiChar(helpfilename) ,HH_HELP_CONTEXT,dword(id));
  Writeln('Htmlhelp returned ',res,', program now blocked on readln, press enter (in console window) to continue');
  readln;
end.
  