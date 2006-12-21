{$mode objfpc}

uses
{$ifdef unix}
 cthreads,
{$endif}
  sysutils,
  classes;

type
  tc = class(tthread)
    procedure execute; override;
  end;

var
  event: pEventState;
  waiting: boolean;

procedure tc.execute;
begin
  { avoid deadlocks/bugs from causing this test to never quit }
  sleep(1000*20);
  halt(1);
end;


begin
  tc.create(false);
  event := BasicEventCreate(nil,false,false,'bla');;
  basiceventSetEvent(event);
  if (basiceventWaitFor(cardinal(-1),event) <> 0) then
    begin
      writeln('error');
      halt(1);
    end;
  { shouldn't change anything }
  basiceventResetEvent(event);
  basiceventSetEvent(event);
  { shouldn't change anything }
  basiceventSetEvent(event);
  if (basiceventWaitFor(cardinal(-1),event) <> 0) then
    begin
      writeln('error');
      halt(1);
    end;
end.
