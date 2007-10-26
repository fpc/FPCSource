{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp18 = double;
{$else FPC_COMP_IS_INT64}
  comp18 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test18(var a); overload;
  begin
    writeln('formal called instead of widechar');
  end;

procedure test18(a: widechar); overload;
  begin
    writeln('widechar called instead of formal');
    halt(1)
  end;

var
  x18: longint;

  y18: widechar;
procedure dotest18;
var
  v: variant;

begin
  try
    v := x18;
    test18(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y18;
    test18(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest18;
end. {$endif not bigfile}
