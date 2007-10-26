{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp110 = double;
{$else FPC_COMP_IS_INT64}
  comp110 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test110(a: smallint); overload;
  begin
    writeln('smallint called instead of single');
  end;

procedure test110(a: single); overload;
  begin
    writeln('single called instead of smallint');
    halt(1)
  end;

var
  x110: smallint;

  y110: single;
procedure dotest110;
var
  v: variant;

begin
  try
    v := x110;
    test110(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y110;
    test110(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest110;
end. {$endif not bigfile}
