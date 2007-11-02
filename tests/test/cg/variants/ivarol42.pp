{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp42 = double;
{$else FPC_COMP_IS_INT64}
  comp42 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test42(a: int64); overload;
  begin
    writeln('int64 called instead of shortint');
    halt(1)
  end;

procedure test42(a: shortint); overload;
  begin
    writeln('shortint called instead of int64');
  end;

var
  x42: int64;

  y42: shortint;
procedure dotest42;
var
  v: variant;

begin
  try
    v := x42;
    test42(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y42;
    test42(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest42;
end. {$endif not bigfile}
