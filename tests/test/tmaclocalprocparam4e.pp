{$modeswitch nestedprocvars}

function test(l: longint): longint;
begin
  test:=l*2;
end;

const
  pp: function(l: longint): longint is nested = @test;

begin
  if pp(6)<>12 then
    halt(1);
end.
