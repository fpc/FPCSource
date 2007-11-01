{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp31 = currency;
{$else FPC_COMP_IS_INT64}
  comp31 = comp;
{$endif FPC_COMP_IS_INT64}
{$ifdef FPC_HAS_TYPE_EXTENDED}
procedure test31(a: comp31); overload;
  begin
    writeln('comp31 called instead of extended');
  end;

procedure test31(a: extended); overload;
  begin
    writeln('extended called instead of comp31');
    halt(1)
  end;

var
  x31: comp31;

  y31: extended;
procedure dotest31;
var
  v: variant;

begin
  try
    v := x31;
    test31(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y31;
    test31(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest31;
end. {$endif not bigfile}
{$else FPC_HAS_TYPE_EXTENDED}
begin
end.
{$endif FPC_HAS_TYPE_EXTENDED}
