{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp158 = double;
{$else FPC_COMP_IS_INT64}
  comp158 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test158(a: double); overload;
  begin
    writeln('double called instead of char');
  end;

procedure test158(a: char); overload;
  begin
    writeln('char called instead of double');
    halt(1)
  end;

var
  x158: double;

  y158: char;
procedure dotest158;
var
  v: variant;

begin
  try
    v := x158;
    test158(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y158;
    test158(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest158;
end. {$endif not bigfile}
