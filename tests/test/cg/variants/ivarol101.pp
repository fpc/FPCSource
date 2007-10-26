{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp101 = currency;
{$else FPC_COMP_IS_INT64}
  comp101 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test101(a: word); overload;
  begin
    writeln('word called instead of extended');
  end;

procedure test101(a: extended); overload;
  begin
    writeln('extended called instead of word');
    halt(1)
  end;

var
  x101: word;

  y101: extended;
procedure dotest101;
var
  v: variant;

begin
  try
    v := x101;
    test101(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y101;
    test101(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest101;
end. {$endif not bigfile}
