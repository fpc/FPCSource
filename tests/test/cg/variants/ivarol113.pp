{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp113 = double;
{$else FPC_COMP_IS_INT64}
  comp113 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test113(a: smallint); overload;
  begin
    writeln('smallint called instead of char');
  end;

procedure test113(a: char); overload;
  begin
    writeln('char called instead of smallint');
    halt(1)
  end;

var
  x113: smallint;

  y113: char;
procedure dotest113;
var
  v: variant;

begin
  try
    v := x113;
    test113(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y113;
    test113(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest113;
end. {$endif not bigfile}
