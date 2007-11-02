{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp164 = double;
{$else FPC_COMP_IS_INT64}
  comp164 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test164(a: extended); overload;
  begin
    writeln('extended called instead of widestring');
  end;

procedure test164(a: widestring); overload;
  begin
    writeln('widestring called instead of extended');
    halt(1)
  end;

var
  x164: extended;

  y164: widestring;
procedure dotest164;
var
  v: variant;

begin
  try
    v := x164;
    test164(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y164;
    test164(v);
  except
    on E : TObject do
      writeln('VVV');
  end;
end;

{$ifndef bigfile} begin
  dotest164;
end. {$endif not bigfile}
