{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp57 = double;
{$else FPC_COMP_IS_INT64}
  comp57 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test57(a: currency); overload;
  begin
    writeln('currency called instead of shortint');
    halt(1)
  end;

procedure test57(a: shortint); overload;
  begin
    writeln('shortint called instead of currency');
  end;

var
  x57: currency;

  y57: shortint;
procedure dotest57;
var
  v: variant;

begin
  try
    v := x57;
    test57(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y57;
    test57(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest57;
end. {$endif not bigfile}
