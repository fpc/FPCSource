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
  comp61 = currency;
{$else FPC_COMP_IS_INT64}
  comp61 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test61(a: currency); overload;
  begin
    writeln('currency called instead of double');
    writeln('XXX')
  end;

procedure test61(a: double); overload;
  begin
    writeln('double called instead of currency');
    writeln('YYY')
  end;

var
  x61: currency;

  y61: double;
procedure dotest61;
var
  v: variant;

begin
  try
    v := x61;
    test61(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y61;
    test61(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest61;
end. {$endif not bigfile}
