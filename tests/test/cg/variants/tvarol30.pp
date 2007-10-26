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
  comp30 = currency;
{$else FPC_COMP_IS_INT64}
  comp30 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test30(a: comp30); overload;
  begin
    writeln('comp30 called instead of double');
    writeln('XXX')
  end;

procedure test30(a: double); overload;
  begin
    writeln('double called instead of comp30');
    writeln('YYY')
  end;

var
  x30: comp30;

  y30: double;
procedure dotest30;
var
  v: variant;

begin
  try
    v := x30;
    test30(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y30;
    test30(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest30;
end. {$endif not bigfile}
