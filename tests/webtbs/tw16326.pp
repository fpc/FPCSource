{$mode objfpc}
{$H+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  classes
  ;

type
  tthread1 = class(tthread)
  public
    procedure execute; override;
  end;

procedure tthread1.execute;
begin
end;

var
  thread1: tthread1;
begin
  thread1 := tthread1.create(true);
  thread1.start;
  thread1.waitfor;
  thread1.free;
end.
