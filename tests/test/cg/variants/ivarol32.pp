{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp32 = double;
{$else FPC_COMP_IS_INT64}
  comp32 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test32(a: comp32); overload;
  begin
    writeln('comp32 called instead of char');
  end;

procedure test32(a: char); overload;
  begin
    writeln('char called instead of comp32');
    halt(1)
  end;

var
  x32: comp32;

  y32: char;
procedure dotest32;
var
  v: variant;

begin
  try
    v := x32;
    test32(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y32;
    test32(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest32;
end. {$endif not bigfile}
