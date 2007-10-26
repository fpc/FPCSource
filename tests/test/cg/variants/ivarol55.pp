{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp55 = double;
{$else FPC_COMP_IS_INT64}
  comp55 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test55(a: currency); overload;
  begin
    writeln('currency called instead of smallint');
    halt(1)
  end;

procedure test55(a: smallint); overload;
  begin
    writeln('smallint called instead of currency');
  end;

var
  x55: currency;

  y55: smallint;
procedure dotest55;
var
  v: variant;

begin
  try
    v := x55;
    test55(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y55;
    test55(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest55;
end. {$endif not bigfile}
