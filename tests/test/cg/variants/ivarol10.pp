{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp10 = double;
{$else FPC_COMP_IS_INT64}
  comp10 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test10(var a); overload;
  begin
    writeln('formal called instead of shortstring');
  end;

procedure test10(a: shortstring); overload;
  begin
    writeln('shortstring called instead of formal');
    halt(1)
  end;

var
  x10: longint;

  y10: shortstring;
procedure dotest10;
var
  v: variant;

begin
  try
    v := x10;
    test10(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y10;
    test10(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest10;
end. {$endif not bigfile}
