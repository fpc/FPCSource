{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp143 = double;
{$else FPC_COMP_IS_INT64}
  comp143 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test143(a: shortstring); overload;
  begin
    writeln('shortstring called instead of widechar');
  end;

procedure test143(a: widechar); overload;
  begin
    writeln('widechar called instead of shortstring');
    halt(1)
  end;

var
  x143: shortstring;

  y143: widechar;
procedure dotest143;
var
  v: variant;

begin
  try
    v := x143;
    test143(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y143;
    test143(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest143;
end. {$endif not bigfile}
