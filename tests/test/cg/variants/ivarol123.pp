{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp123 = double;
{$else FPC_COMP_IS_INT64}
  comp123 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test123(a: byte); overload;
  begin
    writeln('byte called instead of char');
  end;

procedure test123(a: char); overload;
  begin
    writeln('char called instead of byte');
    halt(1)
  end;

var
  x123: byte;

  y123: char;
procedure dotest123;
var
  v: variant;

begin
  try
    v := x123;
    test123(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y123;
    test123(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest123;
end. {$endif not bigfile}
