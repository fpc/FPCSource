{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp134 = double;
{$else FPC_COMP_IS_INT64}
  comp134 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test134(a: shortint); overload;
  begin
    writeln('shortint called instead of widestring');
  end;

procedure test134(a: widestring); overload;
  begin
    writeln('widestring called instead of shortint');
    halt(1)
  end;

var
  x134: shortint;

  y134: widestring;
procedure dotest134;
var
  v: variant;

begin
  try
    v := x134;
    test134(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y134;
    test134(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest134;
end. {$endif not bigfile}
