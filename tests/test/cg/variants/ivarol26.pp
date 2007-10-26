{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp26 = double;
{$else FPC_COMP_IS_INT64}
  comp26 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test26(a: comp26); overload;
  begin
    writeln('comp26 called instead of shortint');
    halt(1)
  end;

procedure test26(a: shortint); overload;
  begin
    writeln('shortint called instead of comp26');
  end;

var
  x26: comp26;

  y26: shortint;
procedure dotest26;
var
  v: variant;

begin
  try
    v := x26;
    test26(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y26;
    test26(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest26;
end. {$endif not bigfile}
