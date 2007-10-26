{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp52 = double;
{$else FPC_COMP_IS_INT64}
  comp52 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test52(a: currency); overload;
  begin
    writeln('currency called instead of longint');
    halt(1)
  end;

procedure test52(a: longint); overload;
  begin
    writeln('longint called instead of currency');
  end;

var
  x52: currency;

  y52: longint;
procedure dotest52;
var
  v: variant;

begin
  try
    v := x52;
    test52(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y52;
    test52(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest52;
end. {$endif not bigfile}
