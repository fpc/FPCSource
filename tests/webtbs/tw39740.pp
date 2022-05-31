{ %NORUN }

program tw39740;

{$mode delphi} 
{$modeswitch FUNCTIONREFERENCES}
uses classes;


{$if not declared(TThreadProcedure)}
type 
   TThreadProcedure = reference to procedure;
{$endif}

procedure bla(t : tthreadmethod); overload;
begin
end;

procedure bla(t : tthreadprocedure); overload;
begin
end; 

type ta = class
            procedure bla;
            end;
 
procedure ta.bla;
begin
end;
var a : Ta;
begin
  bla(a.bla);
  // bla(tthreadmethod(a.bla)); // works
end. 
