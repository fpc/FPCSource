{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp22 = double;
{$else FPC_COMP_IS_INT64}
  comp22 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test22(a: comp22); overload;
  begin
    writeln('comp22 called instead of cardinal');
    halt(1)
  end;

procedure test22(a: cardinal); overload;
  begin
    writeln('cardinal called instead of comp22');
  end;

var
  x22: comp22;

  y22: cardinal;
procedure dotest22;
var
  v: variant;

begin
  try
    v := x22;
    test22(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y22;
    test22(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest22;
end. {$endif not bigfile}
