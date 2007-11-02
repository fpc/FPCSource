{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp38 = double;
{$else FPC_COMP_IS_INT64}
  comp38 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test38(a: int64); overload;
  begin
    writeln('int64 called instead of cardinal');
    halt(1)
  end;

procedure test38(a: cardinal); overload;
  begin
    writeln('cardinal called instead of int64');
  end;

var
  x38: int64;

  y38: cardinal;
procedure dotest38;
var
  v: variant;

begin
  try
    v := x38;
    test38(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y38;
    test38(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest38;
end. {$endif not bigfile}
