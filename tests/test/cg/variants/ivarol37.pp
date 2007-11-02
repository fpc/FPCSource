{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp37 = double;
{$else FPC_COMP_IS_INT64}
  comp37 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test37(a: int64); overload;
  begin
    writeln('int64 called instead of longint');
    halt(1)
  end;

procedure test37(a: longint); overload;
  begin
    writeln('longint called instead of int64');
  end;

var
  x37: int64;

  y37: longint;
procedure dotest37;
var
  v: variant;

begin
  try
    v := x37;
    test37(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y37;
    test37(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest37;
end. {$endif not bigfile}
