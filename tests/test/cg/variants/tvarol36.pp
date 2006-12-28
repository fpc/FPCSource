{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: int64); overload;
  begin
    writeln('int64 called instead of currency');
    halt(1)
  end;

procedure test(a: currency); overload;
  begin
    writeln('currency called instead of int64');
    writeln('YYY')
  end;

var
  v: variant;
  x: int64;
  y: currency;

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
