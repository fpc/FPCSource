{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp77 = double;
{$else FPC_COMP_IS_INT64}
  comp77 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test77(a: longint); overload;
  begin
    writeln('longint called instead of char');
  end;

procedure test77(a: char); overload;
  begin
    writeln('char called instead of longint');
    halt(1)
  end;

var
  x77: longint;

  y77: char;
procedure dotest77;
var
  v: variant;

begin
  try
    v := x77;
    test77(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y77;
    test77(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest77;
end. {$endif not bigfile}
