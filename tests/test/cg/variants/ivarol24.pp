{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp24 = double;
{$else FPC_COMP_IS_INT64}
  comp24 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test24(a: comp24); overload;
  begin
    writeln('comp24 called instead of smallint');
    halt(1)
  end;

procedure test24(a: smallint); overload;
  begin
    writeln('smallint called instead of comp24');
  end;

var
  x24: comp24;

  y24: smallint;
procedure dotest24;
var
  v: variant;

begin
  try
    v := x24;
    test24(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y24;
    test24(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest24;
end. {$endif not bigfile}
