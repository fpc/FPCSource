{ %recompile }

{$modeswitch nestedprocvars}
program tmaclocalprocparam3;

uses
  umaclocalprocparam3f;

var
  tempp: tnestedprocvar2;

  procedure finalglobal;
    begin
      writeln('final');
    end;

  procedure p1( pp: tnestedprocvar2; p: tnestedprocvar);
  begin
    tempp:=pp;
    tempp(p);
  end;

  procedure p2( pp: tnestedprocvar2; p: tnestedprocvar);
  var
    localpp: tnestedprocvar2;
  begin
    localpp:=pp;
    p1( localpp, p)
  end;

  procedure n(pp: tnestedprocvar);
  begin
    writeln( 'calling through n');
    pp();
  end;

  procedure q;
  var qi: longint;

    procedure r(pp: tnestedprocvar);
    begin
      if qi = 1 then
        writeln( 'success for r')
      else
        begin
        writeln( 'fail');
        halt( 1)
      end;
      pp();
    end;

  begin
    qi:= 1;
    p1( @r, @finalglobal);
    p2( @r, @finalglobal);
    p1( @n, @finalglobal);
    p2( @n, @finalglobal);
  end;

begin
	q
end.
