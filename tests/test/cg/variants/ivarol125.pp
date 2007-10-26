{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp125 = double;
{$else FPC_COMP_IS_INT64}
  comp125 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test125(a: byte); overload;
  begin
    writeln('byte called instead of widestring');
  end;

procedure test125(a: widestring); overload;
  begin
    writeln('widestring called instead of byte');
    halt(1)
  end;

var
  x125: byte;

  y125: widestring;
procedure dotest125;
var
  v: variant;

begin
  try
    v := x125;
    test125(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y125;
    test125(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest125;
end. {$endif not bigfile}
