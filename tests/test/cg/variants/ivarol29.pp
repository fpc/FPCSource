{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp29 = double;
{$else FPC_COMP_IS_INT64}
  comp29 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test29(a: comp29); overload;
  begin
    writeln('comp29 called instead of single');
    halt(1)
  end;

procedure test29(a: single); overload;
  begin
    writeln('single called instead of comp29');
  end;

var
  x29: comp29;

  y29: single;
procedure dotest29;
var
  v: variant;

begin
  try
    v := x29;
    test29(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y29;
    test29(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest29;
end. {$endif not bigfile}
