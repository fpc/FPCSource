{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp17 = double;
{$else FPC_COMP_IS_INT64}
  comp17 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test17(var a); overload;
  begin
    writeln('formal called instead of widestring');
  end;

procedure test17(a: widestring); overload;
  begin
    writeln('widestring called instead of formal');
    halt(1)
  end;

var
  x17: longint;

  y17: widestring;
procedure dotest17;
var
  v: variant;

begin
  try
    v := x17;
    test17(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y17;
    test17(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest17;
end. {$endif not bigfile}
