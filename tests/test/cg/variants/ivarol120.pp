{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp120 = double;
{$else FPC_COMP_IS_INT64}
  comp120 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test120(a: byte); overload;
  begin
    writeln('byte called instead of single');
  end;

procedure test120(a: single); overload;
  begin
    writeln('single called instead of byte');
    halt(1)
  end;

var
  x120: byte;

  y120: single;
procedure dotest120;
var
  v: variant;

begin
  try
    v := x120;
    test120(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y120;
    test120(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest120;
end. {$endif not bigfile}
