{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp118 = double;
{$else FPC_COMP_IS_INT64}
  comp118 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test118(a: byte); overload;
  begin
    writeln('byte called instead of shortstring');
  end;

procedure test118(a: shortstring); overload;
  begin
    writeln('shortstring called instead of byte');
    halt(1)
  end;

var
  x118: byte;

  y118: shortstring;
procedure dotest118;
var
  v: variant;

begin
  try
    v := x118;
    test118(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y118;
    test118(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest118;
end. {$endif not bigfile}
