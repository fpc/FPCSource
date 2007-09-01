{ %OPT=-gh }
{ The only problem is that we don't really get a
  non zero exitcode if some memory is not freed PM }
{ now we do, thanks to a patch of yours several years later :) }
{ Source provided for Free Pascal Bug Report 1433 }
{ Submitted by "Aleksey V. Vaneev" on  2001-03-10 }
{ e-mail: picoder@sbis.komi.ru }

{$mode objfpc}

function ExitTest1: Boolean;
var
        aa: AnsiString;

begin
        exit (False);

        aa := 'BUMBUM';
end;

function ExitTest2: Boolean;
var
        aa: AnsiString;

begin
        Result := False;
        exit;

        aa := 'MUBMUB';
end;

begin
        HaltOnNotReleased := true;
        ExitTest1;
        ExitTest2;
end.
