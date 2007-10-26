{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp25 = double;
{$else FPC_COMP_IS_INT64}
  comp25 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test25(a: comp25); overload;
  begin
    writeln('comp25 called instead of byte');
    halt(1)
  end;

procedure test25(a: byte); overload;
  begin
    writeln('byte called instead of comp25');
  end;

var
  x25: comp25;

  y25: byte;
procedure dotest25;
var
  v: variant;

begin
  try
    v := x25;
    test25(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y25;
    test25(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest25;
end. {$endif not bigfile}
