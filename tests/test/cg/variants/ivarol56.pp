{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp56 = double;
{$else FPC_COMP_IS_INT64}
  comp56 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test56(a: currency); overload;
  begin
    writeln('currency called instead of byte');
    halt(1)
  end;

procedure test56(a: byte); overload;
  begin
    writeln('byte called instead of currency');
  end;

var
  x56: currency;

  y56: byte;
procedure dotest56;
var
  v: variant;

begin
  try
    v := x56;
    test56(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y56;
    test56(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest56;
end. {$endif not bigfile}
