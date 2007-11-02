{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp90 = double;
{$else FPC_COMP_IS_INT64}
  comp90 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test90(a: cardinal); overload;
  begin
    writeln('cardinal called instead of char');
  end;

procedure test90(a: char); overload;
  begin
    writeln('char called instead of cardinal');
    halt(1)
  end;

var
  x90: cardinal;

  y90: char;
procedure dotest90;
var
  v: variant;

begin
  try
    v := x90;
    test90(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y90;
    test90(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest90;
end. {$endif not bigfile}
