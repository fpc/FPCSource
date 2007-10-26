{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp49 = double;
{$else FPC_COMP_IS_INT64}
  comp49 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test49(a: int64); overload;
  begin
    writeln('int64 called instead of boolean');
    halt(1)
  end;

procedure test49(a: boolean); overload;
  begin
    writeln('boolean called instead of int64');
  end;

var
  x49: int64;

  y49: boolean;
procedure dotest49;
var
  v: variant;

begin
  try
    v := x49;
    test49(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y49;
    test49(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest49;
end. {$endif not bigfile}
