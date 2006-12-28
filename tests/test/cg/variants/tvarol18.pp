{$ifdef fpc}
{$mode delphi}
{$endif fpc}

{$ifdef FPC_COMP_IS_INT64}
type 
  comp = double;
{$endif FPC_COMP_IS_INT64}
procedure test(var a); overload;
  begin
    writeln('formal called instead of widechar');
    writeln('XXX')
  end;

procedure test(a: widechar); overload;
  begin
    writeln('widechar called instead of formal');
    halt(1)
  end;

var
  v: variant;
  x: longint;
  y: widechar;

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
