{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp63 = double;
{$else FPC_COMP_IS_INT64}
  comp63 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test63(a: currency); overload;
  begin
    writeln('currency called instead of char');
  end;

procedure test63(a: char); overload;
  begin
    writeln('char called instead of currency');
    halt(1)
  end;

var
  x63: currency;

  y63: char;
procedure dotest63;
var
  v: variant;

begin
  try
    v := x63;
    test63(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y63;
    test63(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest63;
end. {$endif not bigfile}
