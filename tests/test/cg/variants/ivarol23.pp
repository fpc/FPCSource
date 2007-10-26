{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp23 = double;
{$else FPC_COMP_IS_INT64}
  comp23 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test23(a: comp23); overload;
  begin
    writeln('comp23 called instead of word');
    halt(1)
  end;

procedure test23(a: word); overload;
  begin
    writeln('word called instead of comp23');
  end;

var
  x23: comp23;

  y23: word;
procedure dotest23;
var
  v: variant;

begin
  try
    v := x23;
    test23(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y23;
    test23(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest23;
end. {$endif not bigfile}
