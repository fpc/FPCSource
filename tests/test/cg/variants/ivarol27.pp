{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp27 = double;
{$else FPC_COMP_IS_INT64}
  comp27 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test27(a: comp27); overload;
  begin
    writeln('comp27 called instead of shortstring');
  end;

procedure test27(a: shortstring); overload;
  begin
    writeln('shortstring called instead of comp27');
    halt(1)
  end;

var
  x27: comp27;

  y27: shortstring;
procedure dotest27;
var
  v: variant;

begin
  try
    v := x27;
    test27(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y27;
    test27(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest27;
end. {$endif not bigfile}
