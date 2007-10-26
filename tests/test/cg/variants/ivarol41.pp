{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp41 = double;
{$else FPC_COMP_IS_INT64}
  comp41 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test41(a: int64); overload;
  begin
    writeln('int64 called instead of byte');
    halt(1)
  end;

procedure test41(a: byte); overload;
  begin
    writeln('byte called instead of int64');
  end;

var
  x41: int64;

  y41: byte;
procedure dotest41;
var
  v: variant;

begin
  try
    v := x41;
    test41(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y41;
    test41(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest41;
end. {$endif not bigfile}
