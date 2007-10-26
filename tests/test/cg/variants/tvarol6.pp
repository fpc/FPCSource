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
  comp6 = double;
{$else FPC_COMP_IS_INT64}
  comp6 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test6(var a); overload;
  begin
    writeln('formal called instead of word');
    writeln('XXX')
  end;

procedure test6(a: word); overload;
  begin
    writeln('word called instead of formal');
    writeln('YYY')
  end;

var
  x6: longint;

  y6: word;
procedure dotest6;
var
  v: variant;

begin
  try
    v := x6;
    test6(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y6;
    test6(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest6;
end. {$endif not bigfile}
