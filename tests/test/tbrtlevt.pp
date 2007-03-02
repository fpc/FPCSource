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
  { make sure we don't exit before this thread has initialised, since    }
  { it can allocate memory in its initialisation, which would cause      }
  { problems for heaptrc as it goes over the memory map in its exit code }
  waiting:=true;
  { avoid deadlocks/bugs from causing this test to never quit }
  sleep(1000*20);
  halt(1);
end;


begin
  waiting:=false;
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
  basiceventdestroy(event);
  while not waiting do
    sleep(20);
end.
