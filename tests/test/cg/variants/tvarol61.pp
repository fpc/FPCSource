{ %fail }
{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = currency;
{$endif FPC_COMP_IS_INT64}
procedure test(a: currency); overload;
  begin
    writeln('currency called instead of double');
    writeln('XXX')
  end;

procedure test(a: double); overload;
  begin
    writeln('double called instead of currency');
    writeln('YYY')
  end;

var
  v: variant;
  x: currency;
  y: double;

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
