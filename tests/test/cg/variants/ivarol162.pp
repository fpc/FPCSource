{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp162 = double;
{$else FPC_COMP_IS_INT64}
  comp162 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test162(a: extended); overload;
  begin
    writeln('extended called instead of char');
  end;

procedure test162(a: char); overload;
  begin
    writeln('char called instead of extended');
    halt(1)
  end;

var
  x162: extended;

  y162: char;
procedure dotest162;
var
  v: variant;

begin
  try
    v := x162;
    test162(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y162;
    test162(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest162;
end. {$endif not bigfile}
