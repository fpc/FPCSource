{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp54 = double;
{$else FPC_COMP_IS_INT64}
  comp54 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test54(a: currency); overload;
  begin
    writeln('currency called instead of word');
    halt(1)
  end;

procedure test54(a: word); overload;
  begin
    writeln('word called instead of currency');
  end;

var
  x54: currency;

  y54: word;
procedure dotest54;
var
  v: variant;

begin
  try
    v := x54;
    test54(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y54;
    test54(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest54;
end. {$endif not bigfile}
