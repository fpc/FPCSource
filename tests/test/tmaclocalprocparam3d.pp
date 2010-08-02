{$modeswitch nestedprocvars}
program tmaclocalprocparam3;

type
  tnestedprocvar = procedure is nested;

var
  tempp: tnestedprocvar;

  procedure p1( pp: tnestedprocvar);
  begin
    tempp:=pp;
    tempp
  end;

  procedure p2( pp: tnestedprocvar);
  var
    localpp: tnestedprocvar;
  begin
    localpp:=pp;
    p1( localpp)
  end;

  procedure n;
  begin
    writeln( 'calling through n')
  end;

  procedure q;
  var qi: longint;

    procedure r;
    begin
      if qi = 1 then
        writeln( 'success for r')
      else
        begin
        writeln( 'fail');
        halt( 1)
      end
    end;

  begin
    qi:= 1;
    p1( @r);
    p2( @r);
    p1( @n);
    p2( @n);
  end;

begin
	q
end.