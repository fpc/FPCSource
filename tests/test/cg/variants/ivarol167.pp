{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp167 = double;
{$else FPC_COMP_IS_INT64}
  comp167 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test167(a: char); overload;
  begin
    writeln('char called instead of widestring');
    halt(1)
  end;

procedure test167(a: widestring); overload;
  begin
    writeln('widestring called instead of char');
  end;

var
  x167: char;

  y167: widestring;
procedure dotest167;
var
  v: variant;

begin
  try
    v := x167;
    test167(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y167;
    test167(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest167;
end. {$endif not bigfile}
