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
  comp94 = double;
{$else FPC_COMP_IS_INT64}
  comp94 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test94(a: word); overload;
  begin
    writeln('word called instead of smallint');
    writeln('XXX')
  end;

procedure test94(a: smallint); overload;
  begin
    writeln('smallint called instead of word');
    writeln('YYY')
  end;

var
  x94: word;

  y94: smallint;
procedure dotest94;
var
  v: variant;

begin
  try
    v := x94;
    test94(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y94;
    test94(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest94;
end. {$endif not bigfile}
