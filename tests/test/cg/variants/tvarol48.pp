{ %fail }
{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp48 = double;
{$else FPC_COMP_IS_INT64}
  comp48 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test48(a: int64); overload;
  begin
    writeln('int64 called instead of char');
    writeln('XXX')
  end;

procedure test48(a: char); overload;
  begin
    writeln('char called instead of int64');
    writeln('YYY')
  end;

var
  x48: int64;

  y48: char;
procedure dotest48;
var
  v: variant;

begin
  try
    v := x48;
    test48(v);
  except
    on E : TObject do
      writeln('QQQ');
  end;

  try
    v := y48;
    test48(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest48;
end. {$endif not bigfile}
