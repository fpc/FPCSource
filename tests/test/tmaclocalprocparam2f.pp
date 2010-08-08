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
      var
        si: longint;

      procedure g( ri: longint);
        begin
          if qi = ri then
            writeln( 'success for g')
          else
            begin
              writeln( 'fail');
              halt( 1)
           end
        end;

      begin
        si:=ri;
        p1( g, qi);
        p2( g, qi);
        p1( g, qi);
        p2( g, qi);
      end;

  begin
    qi:= 1;
    r(qi);
  end;

begin
    q
end.
