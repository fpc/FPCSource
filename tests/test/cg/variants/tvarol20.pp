{ %fail }
{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp20 = double;
{$else FPC_COMP_IS_INT64}
  comp20 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test20(a: comp20); overload;
  begin
    writeln('comp20 called instead of currency');
    writeln('XXX')
  end;

procedure test20(a: currency); overload;
  begin
    writeln('currency called instead of comp20');
    writeln('YYY')
  end;

var
  x20: comp20;

  y20: currency;
procedure dotest20;
var
  v: variant;

begin
  try
    v := x20;
    test20(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y20;
    test20(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest20;
end. {$endif not bigfile}
