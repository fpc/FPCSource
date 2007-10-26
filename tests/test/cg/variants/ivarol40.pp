{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp40 = double;
{$else FPC_COMP_IS_INT64}
  comp40 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test40(a: int64); overload;
  begin
    writeln('int64 called instead of smallint');
    halt(1)
  end;

procedure test40(a: smallint); overload;
  begin
    writeln('smallint called instead of int64');
  end;

var
  x40: int64;

  y40: smallint;
procedure dotest40;
var
  v: variant;

begin
  try
    v := x40;
    test40(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y40;
    test40(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest40;
end. {$endif not bigfile}
