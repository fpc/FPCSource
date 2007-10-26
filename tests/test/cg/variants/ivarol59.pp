{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp59 = double;
{$else FPC_COMP_IS_INT64}
  comp59 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test59(a: currency); overload;
  begin
    writeln('currency called instead of ansistring');
  end;

procedure test59(a: ansistring); overload;
  begin
    writeln('ansistring called instead of currency');
    halt(1)
  end;

var
  x59: currency;

  y59: ansistring;
procedure dotest59;
var
  v: variant;

begin
  try
    v := x59;
    test59(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y59;
    test59(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest59;
end. {$endif not bigfile}
