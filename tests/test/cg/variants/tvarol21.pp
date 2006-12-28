{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: comp); overload;
  begin
    writeln('comp called instead of longint');
    halt(1)
  end;

procedure test(a: longint); overload;
  begin
    writeln('longint called instead of comp');
    writeln('YYY')
  end;

var
  v: variant;
  x: comp;
  y: longint;

begin
  try
    v := x;
    test(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y;
    test(v);
  except
    on E : TObject do
      halt(1);
  end;
end.
