{ %fail }
{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(var a); overload;
  begin
    writeln('formal called instead of comp');
    writeln('XXX')
  end;

procedure test(a: comp); overload;
  begin
    writeln('comp called instead of formal');
    writeln('YYY')
  end;

var
  v: variant;
  x: longint;
  y: comp;

begin
  try
    v := x;
    test(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y;
    test(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end.
