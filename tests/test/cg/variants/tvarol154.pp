{ %fail }
{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: single); overload;
  begin
    writeln('single called instead of boolean');
    writeln('XXX')
  end;

procedure test(a: boolean); overload;
  begin
    writeln('boolean called instead of single');
    writeln('YYY')
  end;

var
  v: variant;
  x: single;
  y: boolean;

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
