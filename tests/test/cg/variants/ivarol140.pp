{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp140 = double;
{$else FPC_COMP_IS_INT64}
  comp140 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test140(a: shortstring); overload;
  begin
    writeln('shortstring called instead of char');
  end;

procedure test140(a: char); overload;
  begin
    writeln('char called instead of shortstring');
    halt(1)
  end;

var
  x140: shortstring;

  y140: char;
procedure dotest140;
var
  v: variant;

begin
  try
    v := x140;
    test140(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y140;
    test140(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest140;
end. {$endif not bigfile}
