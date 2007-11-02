{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp11 = double;
{$else FPC_COMP_IS_INT64}
  comp11 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test11(var a); overload;
  begin
    writeln('formal called instead of ansistring');
  end;

procedure test11(a: ansistring); overload;
  begin
    writeln('ansistring called instead of formal');
    halt(1)
  end;

var
  x11: longint;

  y11: ansistring;
procedure dotest11;
var
  v: variant;

begin
  try
    v := x11;
    test11(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y11;
    test11(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest11;
end. {$endif not bigfile}
