{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp15 = double;
{$else FPC_COMP_IS_INT64}
  comp15 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test15(var a); overload;
  begin
    writeln('formal called instead of char');
  end;

procedure test15(a: char); overload;
  begin
    writeln('char called instead of formal');
    halt(1)
  end;

var
  x15: longint;

  y15: char;
procedure dotest15;
var
  v: variant;

begin
  try
    v := x15;
    test15(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y15;
    test15(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest15;
end. {$endif not bigfile}
