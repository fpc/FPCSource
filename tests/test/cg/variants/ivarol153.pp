{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp153 = double;
{$else FPC_COMP_IS_INT64}
  comp153 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test153(a: single); overload;
  begin
    writeln('single called instead of char');
  end;

procedure test153(a: char); overload;
  begin
    writeln('char called instead of single');
    halt(1)
  end;

var
  x153: single;

  y153: char;
procedure dotest153;
var
  v: variant;

begin
  try
    v := x153;
    test153(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y153;
    test153(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest153;
end. {$endif not bigfile}
