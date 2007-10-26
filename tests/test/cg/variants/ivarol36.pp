{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp36 = double;
{$else FPC_COMP_IS_INT64}
  comp36 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test36(a: int64); overload;
  begin
    writeln('int64 called instead of currency');
    halt(1)
  end;

procedure test36(a: currency); overload;
  begin
    writeln('currency called instead of int64');
  end;

var
  x36: int64;

  y36: currency;
procedure dotest36;
var
  v: variant;

begin
  try
    v := x36;
    test36(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y36;
    test36(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest36;
end. {$endif not bigfile}
