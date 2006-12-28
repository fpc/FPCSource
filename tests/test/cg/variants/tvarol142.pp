{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(a: shortstring); overload;
  begin
    writeln('shortstring called instead of widestring');
    writeln('XXX')
  end;

procedure test(a: widestring); overload;
  begin
    writeln('widestring called instead of shortstring');
    halt(1)
  end;

var
  v: variant;
  x: shortstring;
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
      halt(1);
  end;
end.
