{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp115 = double;
{$else FPC_COMP_IS_INT64}
  comp115 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test115(a: smallint); overload;
  begin
    writeln('smallint called instead of widestring');
  end;

procedure test115(a: widestring); overload;
  begin
    writeln('widestring called instead of smallint');
    halt(1)
  end;

var
  x115: smallint;

  y115: widestring;
procedure dotest115;
var
  v: variant;

begin
  try
    v := x115;
    test115(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y115;
    test115(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest115;
end. {$endif not bigfile}
