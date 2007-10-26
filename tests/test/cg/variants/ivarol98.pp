{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp98 = double;
{$else FPC_COMP_IS_INT64}
  comp98 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test98(a: word); overload;
  begin
    writeln('word called instead of ansistring');
  end;

procedure test98(a: ansistring); overload;
  begin
    writeln('ansistring called instead of word');
    halt(1)
  end;

var
  x98: word;

  y98: ansistring;
procedure dotest98;
var
  v: variant;

begin
  try
    v := x98;
    test98(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y98;
    test98(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest98;
end. {$endif not bigfile}
