{ %OPT=-gl -O- }
{ %TARGET=wince,win32,win64 }
program tw14330;

{$mode objfpc}

uses
  SysUtils;

procedure Fail;

begin
  raise Exception.Create('test');
end;

var
  lTrace: shortstring;

begin
  lTrace:='';
  try
    Fail;
  except
    lTrace:=BackTraceStrFunc(ExceptAddr);
  end;
  writeln(lTrace);
  if pos('tw14330.pp', lTrace)=0 then
    begin
      writeln('Backtrace must report the source file of the main executable');
      halt(1);
    end;
  if pos('FAIL', upcase(lTrace))=0 then
    begin
      writeln('Backtrace must report the raising routine');
      halt(2);
    end;
  writeln('ok');
end.
