{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp19 = double;
{$else FPC_COMP_IS_INT64}
  comp19 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test19(a: comp19); overload;
  begin
    writeln('comp19 called instead of int64');
  end;

procedure test19(a: int64); overload;
  begin
    writeln('int64 called instead of comp19');
    halt(1)
  end;

var
  x19: comp19;

  y19: int64;
procedure dotest19;
var
  v: variant;

begin
  try
    v := x19;
    test19(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y19;
    test19(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest19;
end. {$endif not bigfile}
