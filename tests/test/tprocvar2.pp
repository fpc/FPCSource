{$F+}
{$ifdef fpc}
{$mode tp}
{$endif fpc}

type
  tproc = procedure;
  tprocx = procedure(x : longint);

const
  dummy_call_count : longint = 0;

procedure dummy;
begin
  writeln('Dummy called');
  inc(dummy_call_count);
end;

procedure dummyx(x : longint);
begin
  writeln('Dummy called with x=',x);
  inc(dummy_call_count);
end;

var
  tp2 : tproc;
  tp1x,tp2x : tprocx;
const
  tp1 : tproc = dummy;

begin
  move(@tp1,@tp2,sizeof(tproc));
  tp2;
  tp1x:=dummyx;
  move(@tp1x,@tp2x,sizeof(tproc));
  tp2x(2);
end.
