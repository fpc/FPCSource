{ %NORUN }

program tw39745;

{$mode delphi} // objfpc is ok
{$modeswitch functionreferences}
//uses classes;
type
    TThreadMethod = procedure of object;
    TThreadProcedure = reference to procedure;

    TThread = class
      procedure Queue(aMethod: TThreadMethod);overload;
      procedure Queue(aFuncRef: TThreadProcedure);overload;
    end;

    TTestThread = class(tthread)

                  procedure something; // matches tthreadmethod
                  procedure xx;
               end;

procedure tthread.Queue(aMethod: TThreadMethod);
begin
end;
procedure tthread.Queue(aFuncRef: TThreadProcedure);
begin
end;
procedure ttestthread.something;
begin
end;
procedure ttestthread.xx;
begin
      Queue(tthreadprocedure(Something)); // add @ for mode objfpc
end;

begin
end.

