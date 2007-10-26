{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp58 = double;
{$else FPC_COMP_IS_INT64}
  comp58 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test58(a: currency); overload;
  begin
    writeln('currency called instead of shortstring');
  end;

procedure test58(a: shortstring); overload;
  begin
    writeln('shortstring called instead of currency');
    halt(1)
  end;

var
  x58: currency;

  y58: shortstring;
procedure dotest58;
var
  v: variant;

begin
  try
    v := x58;
    test58(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y58;
    test58(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest58;
end. {$endif not bigfile}
