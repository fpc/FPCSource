{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp100 = currency;
{$else FPC_COMP_IS_INT64}
  comp100 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test100(a: word); overload;
  begin
    writeln('word called instead of double');
  end;

procedure test100(a: double); overload;
  begin
    writeln('double called instead of word');
    halt(1)
  end;

var
  x100: word;

  y100: double;
procedure dotest100;
var
  v: variant;

begin
  try
    v := x100;
    test100(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y100;
    test100(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest100;
end. {$endif not bigfile}
