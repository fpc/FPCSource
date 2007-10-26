{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp97 = double;
{$else FPC_COMP_IS_INT64}
  comp97 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test97(a: word); overload;
  begin
    writeln('word called instead of shortstring');
  end;

procedure test97(a: shortstring); overload;
  begin
    writeln('shortstring called instead of word');
    halt(1)
  end;

var
  x97: word;

  y97: shortstring;
procedure dotest97;
var
  v: variant;

begin
  try
    v := x97;
    test97(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y97;
    test97(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest97;
end. {$endif not bigfile}
