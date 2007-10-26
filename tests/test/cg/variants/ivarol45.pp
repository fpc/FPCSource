{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp45 = double;
{$else FPC_COMP_IS_INT64}
  comp45 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test45(a: int64); overload;
  begin
    writeln('int64 called instead of single');
    halt(1)
  end;

procedure test45(a: single); overload;
  begin
    writeln('single called instead of int64');
  end;

var
  x45: int64;

  y45: single;
procedure dotest45;
var
  v: variant;

begin
  try
    v := x45;
    test45(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y45;
    test45(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest45;
end. {$endif not bigfile}
