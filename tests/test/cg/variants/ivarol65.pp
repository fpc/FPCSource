{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp65 = double;
{$else FPC_COMP_IS_INT64}
  comp65 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test65(a: currency); overload;
  begin
    writeln('currency called instead of widestring');
  end;

procedure test65(a: widestring); overload;
  begin
    writeln('widestring called instead of currency');
    halt(1)
  end;

var
  x65: currency;

  y65: widestring;
procedure dotest65;
var
  v: variant;

begin
  try
    v := x65;
    test65(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y65;
    test65(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest65;
end. {$endif not bigfile}
