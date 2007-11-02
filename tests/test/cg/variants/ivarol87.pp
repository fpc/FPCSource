{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp87 = double;
{$else FPC_COMP_IS_INT64}
  comp87 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test87(a: cardinal); overload;
  begin
    writeln('cardinal called instead of single');
  end;

procedure test87(a: single); overload;
  begin
    writeln('single called instead of cardinal');
    halt(1)
  end;

var
  x87: cardinal;

  y87: single;
procedure dotest87;
var
  v: variant;

begin
  try
    v := x87;
    test87(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y87;
    test87(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest87;
end. {$endif not bigfile}
