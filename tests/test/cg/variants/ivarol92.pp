{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp92 = double;
{$else FPC_COMP_IS_INT64}
  comp92 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test92(a: cardinal); overload;
  begin
    writeln('cardinal called instead of widestring');
  end;

procedure test92(a: widestring); overload;
  begin
    writeln('widestring called instead of cardinal');
    halt(1)
  end;

var
  x92: cardinal;

  y92: widestring;
procedure dotest92;
var
  v: variant;

begin
  try
    v := x92;
    test92(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y92;
    test92(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest92;
end. {$endif not bigfile}
