{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp171 = double;
{$else FPC_COMP_IS_INT64}
  comp171 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test171(a: widestring); overload;
  begin
    writeln('widestring called instead of widechar');
  end;

procedure test171(a: widechar); overload;
  begin
    writeln('widechar called instead of widestring');
    halt(1)
  end;

var
  x171: widestring;

  y171: widechar;
procedure dotest171;
var
  v: variant;

begin
  try
    v := x171;
    test171(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y171;
    test171(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest171;
end. {$endif not bigfile}
