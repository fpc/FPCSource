{$mode objfpc}

uses
  {$ifdef unix}cthreads,{$endif} math, classes;

type
  tmythread = class(tthread)
    constructor create; reintroduce;
    procedure execute; override;
  end;

  tmychildthread = class(tmythread)
    procedure execute; override;
  end;

var
  e: TFPUException;
  expectedfpumask: tfpuexceptionmask;

constructor tmythread.create;
  begin
    inherited create(true);
  end;

procedure tmythread.execute;
  var
    e: TFPUException;
  begin
    write('thread: ');
    for e in GetExceptionMask do
      write(e,', ');
    writeln;
    if GetExceptionMask<>expectedfpumask then
      begin
        writeln(hexstr(cardinal(GetExceptionMask),8));
        writeln(hexstr(cardinal(expectedfpumask),8));
        halt(1);
      end;
    with tmychildthread.create do
      begin
        start;
        waitfor;
        free;
        SetExceptionMask([ExDenormalized]);
        // in case custom masks are not supported, get the actual new mask
        expectedfpumask:=GetExceptionMask;
        write('after setting ExDenormalized mask: ');
        for e in expectedfpumask do
          write(e,', ');
        writeln;
      end;
  end;

procedure tmychildthread.execute;
  var
    e: TFPUException;
  begin
    write('child thread: ');
    for e in GetExceptionMask do
      write(e,', ');
    writeln;
    if GetExceptionMask<>expectedfpumask then
      halt(2);
  end;

begin
    write('main: ');
    for e in GetExceptionMask do
      write(e,', ');
    writeln;
  expectedfpumask:=GetExceptionMask;
  with tmythread.create do
    begin
      start;
      waitfor;
      free;
    end;
  with tmythread.create do
    begin
      start;
      waitfor;
      free;
    end;
end.
