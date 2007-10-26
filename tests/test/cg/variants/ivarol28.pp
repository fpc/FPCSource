{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp28 = double;
{$else FPC_COMP_IS_INT64}
  comp28 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test28(a: comp28); overload;
  begin
    writeln('comp28 called instead of ansistring');
  end;

procedure test28(a: ansistring); overload;
  begin
    writeln('ansistring called instead of comp28');
    halt(1)
  end;

var
  x28: comp28;

  y28: ansistring;
procedure dotest28;
var
  v: variant;

begin
  try
    v := x28;
    test28(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y28;
    test28(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest28;
end. {$endif not bigfile}
