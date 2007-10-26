{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp119 = double;
{$else FPC_COMP_IS_INT64}
  comp119 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test119(a: byte); overload;
  begin
    writeln('byte called instead of ansistring');
  end;

procedure test119(a: ansistring); overload;
  begin
    writeln('ansistring called instead of byte');
    halt(1)
  end;

var
  x119: byte;

  y119: ansistring;
procedure dotest119;
var
  v: variant;

begin
  try
    v := x119;
    test119(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y119;
    test119(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest119;
end. {$endif not bigfile}
