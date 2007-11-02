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
  comp91 = double;
{$else FPC_COMP_IS_INT64}
  comp91 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test91(a: cardinal); overload;
  begin
    writeln('cardinal called instead of boolean');
    writeln('XXX')
  end;

procedure test91(a: boolean); overload;
  begin
    writeln('boolean called instead of cardinal');
    writeln('YYY')
  end;

var
  x91: cardinal;

  y91: boolean;
procedure dotest91;
var
  v: variant;

begin
  try
    v := x91;
    test91(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y91;
    test91(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest91;
end. {$endif not bigfile}
