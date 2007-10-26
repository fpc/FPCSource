{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp160 = double;
{$else FPC_COMP_IS_INT64}
  comp160 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test160(a: double); overload;
  begin
    writeln('double called instead of widestring');
  end;

procedure test160(a: widestring); overload;
  begin
    writeln('widestring called instead of double');
    halt(1)
  end;

var
  x160: double;

  y160: widestring;
procedure dotest160;
var
  v: variant;

begin
  try
    v := x160;
    test160(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y160;
    test160(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest160;
end. {$endif not bigfile}
