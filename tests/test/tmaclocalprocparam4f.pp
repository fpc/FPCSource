{ %fail }

{$modeswitch nestedprocvars}

procedure outer;

  function test(l: longint): longint;
  begin
    test:=l*2;
  end;

{ can't assign nested proc to typed const, requires
  frame pointer }
  const
    pp: function(l: longint): longint is nested = @test;

begin
end;

begin
end.
