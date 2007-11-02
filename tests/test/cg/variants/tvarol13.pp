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
  comp13 = currency;
{$else FPC_COMP_IS_INT64}
  comp13 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test13(var a); overload;
  begin
    writeln('formal called instead of double');
    writeln('XXX')
  end;

procedure test13(a: double); overload;
  begin
    writeln('double called instead of formal');
    writeln('YYY')
  end;

var
  x13: longint;

  y13: double;
procedure dotest13;
var
  v: variant;

begin
  try
    v := x13;
    test13(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y13;
    test13(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest13;
end. {$endif not bigfile}
