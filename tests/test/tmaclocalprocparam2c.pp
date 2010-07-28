{$mode macpas}
program tmaclocalprocparam2;

type
  tnestedprocvar = procedure ( pi: longint) is nested;

  procedure p1(pp: tnestedprocvar ; i: longint);
  begin
    pp( i)
  end;

  procedure p2(pp: tnestedprocvar ; i: longint);
  begin
    p1( pp, i)
  end;

  procedure n( ri: longint);
  begin
    if ri = 1 then
      writeln( 'success for n')
    else
      begin
        writeln( 'fail');
        halt( 1)
      end
   end;

  procedure q;
  var qi: longint;

    procedure r( ri: longint);
    begin
      if qi = ri then
        writeln( 'success for r')
      else
        begin
        writeln( 'fail');
        halt( 1)
      end
    end;

  begin
    qi:= 1;
    p1( r, qi);
    p2( r, qi);
    p1( n, qi);
    p2( n, qi);
  end;

begin
    q
end.
