{ %RESULT=123 }
{ Source provided for Free Pascal Bug Report 2197 }
{ Submitted by "Pavel V.Ozerski" on  2002-10-23 }
{ e-mail: pavel@insect.mail.iephb.ru }

{$ifndef MACOS}
{$APPTYPE CONSOLE}
{$else}
{$APPTYPE TOOL}
{$endif}

{modified sample of Vlad Smaglyuk}
 procedure Average ({const} Row : Array of byte);
  begin
    writeln('Procedure body');
  end;
 BEGIN
     writeln('Before call');
     Average([1,2,3]);
     writeln('After call');
     { We need to be sure that the following exitcode is
       returned }
     runerror(123);
 END.
