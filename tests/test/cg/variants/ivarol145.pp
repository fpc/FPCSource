{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp145 = currency;
{$else FPC_COMP_IS_INT64}
  comp145 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test145(a: ansistring); overload;
  begin
    writeln('ansistring called instead of double');
    halt(1)
  end;

procedure test145(a: double); overload;
  begin
    writeln('double called instead of ansistring');
  end;

var
  x145: ansistring;

  y145: double;
procedure dotest145;
var
  v: variant;

begin
  try
    v := x145;
    test145(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y145;
    test145(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest145;
end. {$endif not bigfile}
