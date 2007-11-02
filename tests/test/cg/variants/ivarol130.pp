{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp130 = currency;
{$else FPC_COMP_IS_INT64}
  comp130 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test130(a: shortint); overload;
  begin
    writeln('shortint called instead of double');
  end;

procedure test130(a: double); overload;
  begin
    writeln('double called instead of shortint');
    halt(1)
  end;

var
  x130: shortint;

  y130: double;
procedure dotest130;
var
  v: variant;

begin
  try
    v := x130;
    test130(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y130;
    test130(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest130;
end. {$endif not bigfile}
