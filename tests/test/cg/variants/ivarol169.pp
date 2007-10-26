{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp169 = double;
{$else FPC_COMP_IS_INT64}
  comp169 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test169(a: boolean); overload;
  begin
    writeln('boolean called instead of widestring');
  end;

procedure test169(a: widestring); overload;
  begin
    writeln('widestring called instead of boolean');
    halt(1)
  end;

var
  x169: boolean;

  y169: widestring;
procedure dotest169;
var
  v: variant;

begin
  try
    v := x169;
    test169(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y169;
    test169(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest169;
end. {$endif not bigfile}
