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
  comp154 = double;
{$else FPC_COMP_IS_INT64}
  comp154 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test154(a: single); overload;
  begin
    writeln('single called instead of boolean');
    writeln('XXX')
  end;

procedure test154(a: boolean); overload;
  begin
    writeln('boolean called instead of single');
    writeln('YYY')
  end;

var
  x154: single;

  y154: boolean;
procedure dotest154;
var
  v: variant;

begin
  try
    v := x154;
    test154(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y154;
    test154(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest154;
end. {$endif not bigfile}
