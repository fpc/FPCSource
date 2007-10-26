{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp127 = double;
{$else FPC_COMP_IS_INT64}
  comp127 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test127(a: shortint); overload;
  begin
    writeln('shortint called instead of shortstring');
  end;

procedure test127(a: shortstring); overload;
  begin
    writeln('shortstring called instead of shortint');
    halt(1)
  end;

var
  x127: shortint;

  y127: shortstring;
procedure dotest127;
var
  v: variant;

begin
  try
    v := x127;
    test127(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y127;
    test127(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest127;
end. {$endif not bigfile}
