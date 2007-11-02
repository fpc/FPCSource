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
  comp84 = double;
{$else FPC_COMP_IS_INT64}
  comp84 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test84(a: cardinal); overload;
  begin
    writeln('cardinal called instead of shortint');
    writeln('XXX')
  end;

procedure test84(a: shortint); overload;
  begin
    writeln('shortint called instead of cardinal');
    writeln('YYY')
  end;

var
  x84: cardinal;

  y84: shortint;
procedure dotest84;
var
  v: variant;

begin
  try
    v := x84;
    test84(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y84;
    test84(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest84;
end. {$endif not bigfile}
