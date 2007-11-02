{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp95 = double;
{$else FPC_COMP_IS_INT64}
  comp95 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test95(a: word); overload;
  begin
    writeln('word called instead of byte');
    halt(1)
  end;

procedure test95(a: byte); overload;
  begin
    writeln('byte called instead of word');
  end;

var
  x95: word;

  y95: byte;
procedure dotest95;
var
  v: variant;

begin
  try
    v := x95;
    test95(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y95;
    test95(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest95;
end. {$endif not bigfile}
