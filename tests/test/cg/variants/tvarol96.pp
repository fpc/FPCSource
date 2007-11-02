{ %fail }
{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp96 = double;
{$else FPC_COMP_IS_INT64}
  comp96 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test96(a: word); overload;
  begin
    writeln('word called instead of shortint');
    writeln('XXX')
  end;

procedure test96(a: shortint); overload;
  begin
    writeln('shortint called instead of word');
    writeln('YYY')
  end;

var
  x96: word;

  y96: shortint;
procedure dotest96;
var
  v: variant;

begin
  try
    v := x96;
    test96(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y96;
    test96(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest96;
end. {$endif not bigfile}
