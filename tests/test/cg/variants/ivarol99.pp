{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp99 = double;
{$else FPC_COMP_IS_INT64}
  comp99 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test99(a: word); overload;
  begin
    writeln('word called instead of single');
  end;

procedure test99(a: single); overload;
  begin
    writeln('single called instead of word');
    halt(1)
  end;

var
  x99: word;

  y99: single;
procedure dotest99;
var
  v: variant;

begin
  try
    v := x99;
    test99(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y99;
    test99(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest99;
end. {$endif not bigfile}
