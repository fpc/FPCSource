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
  comp64 = double;
{$else FPC_COMP_IS_INT64}
  comp64 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test64(a: currency); overload;
  begin
    writeln('currency called instead of boolean');
    writeln('XXX')
  end;

procedure test64(a: boolean); overload;
  begin
    writeln('boolean called instead of currency');
    writeln('YYY')
  end;

var
  x64: currency;

  y64: boolean;
procedure dotest64;
var
  v: variant;

begin
  try
    v := x64;
    test64(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y64;
    test64(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest64;
end. {$endif not bigfile}
