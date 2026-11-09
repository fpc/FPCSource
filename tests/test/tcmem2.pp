{ %target=linux,freebsd,darwin }
{ cmem after cthreads, with a thread raising an exception and calling BackTraceStrFunc, issue #32198 }
program tcmem2;

{$mode objfpc}{$H+}

uses
  cthreads, cmem, Classes, SysUtils;

type
  TRaiseThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Trace: String;
  end;


procedure TRaiseThread.Execute;

begin
  try
    raise Exception.Create('test');
  except
    Trace:=BackTraceStrFunc(ExceptAddr);
  end;
end;


var
  lThread: TRaiseThread;

begin
  lThread:=TRaiseThread.Create(False);
  lThread.WaitFor;
  if lThread.Trace='' then
    halt(1);
  lThread.Free;
  writeln('ok');
end.
