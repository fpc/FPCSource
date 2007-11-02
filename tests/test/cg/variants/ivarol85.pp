{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp85 = double;
{$else FPC_COMP_IS_INT64}
  comp85 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test85(a: cardinal); overload;
  begin
    writeln('cardinal called instead of shortstring');
  end;

procedure test85(a: shortstring); overload;
  begin
    writeln('shortstring called instead of cardinal');
    halt(1)
  end;

var
  x85: cardinal;

  y85: shortstring;
procedure dotest85;
var
  v: variant;

begin
  try
    v := x85;
    test85(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y85;
    test85(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest85;
end. {$endif not bigfile}
