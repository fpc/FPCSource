{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: byte); overload;
  begin
    writeln('byte called instead of widestring');
    writeln('XXX')
  end;

procedure test(a: widestring); overload;
  begin
    writeln('widestring called instead of byte');
    halt(1)
  end;

var
  v: variant;
  x: byte;
  y: widestring;

begin
  try
    v := x;
    test(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y;
    test(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end.
