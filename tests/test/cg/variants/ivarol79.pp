{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp79 = double;
{$else FPC_COMP_IS_INT64}
  comp79 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test79(a: longint); overload;
  begin
    writeln('longint called instead of widestring');
  end;

procedure test79(a: widestring); overload;
  begin
    writeln('widestring called instead of longint');
    halt(1)
  end;

var
  x79: longint;

  y79: widestring;
procedure dotest79;
var
  v: variant;

begin
  try
    v := x79;
    test79(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y79;
    test79(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest79;
end. {$endif not bigfile}
