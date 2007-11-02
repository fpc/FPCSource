{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp83 = double;
{$else FPC_COMP_IS_INT64}
  comp83 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test83(a: cardinal); overload;
  begin
    writeln('cardinal called instead of byte');
    halt(1)
  end;

procedure test83(a: byte); overload;
  begin
    writeln('byte called instead of cardinal');
  end;

var
  x83: cardinal;

  y83: byte;
procedure dotest83;
var
  v: variant;

begin
  try
    v := x83;
    test83(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y83;
    test83(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest83;
end. {$endif not bigfile}
