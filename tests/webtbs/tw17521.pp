{$mode objfpc}
{$h+}


type tx = object
       a,b,c: longint; // remove these => no crash
       constructor init;
       function v: longint; virtual;
       end;
     px = ^tx;
     
constructor tx.init;
  begin
  end;

function tx.v: longint;
  begin
    v:=b;
  end;
  
var t : function:longint of object;
    p : px;
    
begin
  new( p, init );
  p^.a:=3;
  p^.b:=4;
  p^.c:=5;
  p^.v; // ok
  t := @p^.v; // sigsegv
  if t()<>4 then
    halt(1);
end.
