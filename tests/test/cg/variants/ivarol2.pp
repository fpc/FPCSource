{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp2 = double;
{$else FPC_COMP_IS_INT64}
  comp2 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test2(var a); overload;
  begin
    writeln('formal called instead of int64');
  end;

procedure test2(a: int64); overload;
  begin
    writeln('int64 called instead of formal');
    halt(1)
  end;

var
  x2: longint;

  y2: int64;
procedure dotest2;
var
  v: variant;

begin
  try
    v := x2;
    test2(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y2;
    test2(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest2;
end. {$endif not bigfile}
