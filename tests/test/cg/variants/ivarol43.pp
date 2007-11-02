{$ifndef bigfile}
{$ifdef fpc}
{$mode delphi}
{$else fpc}
{$define FPC_HAS_TYPE_EXTENDED}
{$endif fpc}
{$endif bigfile}

type 
{$ifdef FPC_COMP_IS_INT64}
  comp43 = double;
{$else FPC_COMP_IS_INT64}
  comp43 = comp;
{$endif FPC_COMP_IS_INT64}
procedure test43(a: int64); overload;
  begin
    writeln('int64 called instead of shortstring');
    halt(1)
  end;

procedure test43(a: shortstring); overload;
  begin
    writeln('shortstring called instead of int64');
  end;

var
  x43: int64;

  y43: shortstring;
procedure dotest43;
var
  v: variant;

begin
  try
    v := x43;
    test43(v);
  except
    on E : TObject do
      halt(1);
  end;

  try
    v := y43;
    test43(v);
  except
    on E : TObject do
      halt(1);
  end;
end;

{$ifndef bigfile} begin
  dotest43;
end. {$endif not bigfile}
