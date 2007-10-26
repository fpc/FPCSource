{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp111 = currency;
{$else FPC_COMP_IS_INT64}
  comp111 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test111(a: smallint); overload;
  begin
    writeln('smallint called instead of double');
  end;

procedure test111(a: double); overload;
  begin
    writeln('double called instead of smallint');
    halt(1)
  end;

var
  x111: smallint;

  y111: double;
procedure dotest111;
var
  v: variant;

begin
  try
    v := x111;
    test111(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y111;
    test111(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest111;
end. {$endif not bigfile}
