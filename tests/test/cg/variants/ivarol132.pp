{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp132 = double;
{$else FPC_COMP_IS_INT64}
  comp132 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test132(a: shortint); overload;
  begin
    writeln('shortint called instead of char');
  end;

procedure test132(a: char); overload;
  begin
    writeln('char called instead of shortint');
    halt(1)
  end;

var
  x132: shortint;

  y132: char;
procedure dotest132;
var
  v: variant;

begin
  try
    v := x132;
    test132(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y132;
    test132(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest132;
end. {$endif not bigfile}
