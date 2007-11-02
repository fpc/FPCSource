{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp137 = double;
{$else FPC_COMP_IS_INT64}
  comp137 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test137(a: shortstring); overload;
  begin
    writeln('shortstring called instead of single');
    halt(1)
  end;

procedure test137(a: single); overload;
  begin
    writeln('single called instead of shortstring');
  end;

var
  x137: shortstring;

  y137: single;
procedure dotest137;
var
  v: variant;

begin
  try
    v := x137;
    test137(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y137;
    test137(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest137;
end. {$endif not bigfile}
