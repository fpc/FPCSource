{$mode tp}
{$modeswitch nestedprocvars}
program tmaclocalprocparam3;

  procedure p1( procedure pp);
  begin
    pp
  end;

  procedure p2( procedure pp);
  begin
    p1( pp)
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
    p1( r);
    p2( r);
    p1( n);
    p2( n);
  end;

begin
	q
end.
