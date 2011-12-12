program tpvarglobaldelphi;

{$mode delphi}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
  tmprec = record
    b: byte;
  end;

  tmypvar = function(x: longint; w: word; r: tmprec; var ro: tmprec): shortstring;

  function test(x: longint; w: word; r: tmprec; var ro: tmprec): shortstring;
    begin
      jlsystem.fout.print('x: ');
      jlsystem.fout.println(x);
      jlsystem.fout.print('w: ');
      jlsystem.fout.println(w);
      jlsystem.fout.print('r.b: ');
      jlsystem.fout.println(r.b);
      jlsystem.fout.print('ro.b: ');
      jlsystem.fout.println(ro.b);
      if x<>1 then
        raise jlexception.create('x wrong on input');
      if w<>$ffff then
        raise jlexception.create('w wrong on input');
      if r.b<>21 then
        raise jlexception.create('r.b wrong on input');
      if ro.b<>42 then
        raise jlexception.create('ro.b wrong on input');
      r.b:=123;
      ro.b:=123;
      result:='abc';
    end;

var
  mypvar: tmypvar;
  r, ro: tmprec;
  res: shortstring;
begin
  r.b:=21;
  ro.b:=42;
  mypvar:=test;
  res:=mypvar(1,$ffff,r,ro);
  if r.b<>21 then
    raise jlexception.create('r changed');
  if ro.b<>123 then
    raise jlexception.create('ro not changed');
  if res<>'abc' then
    raise jlexception.create('result wrong');
end.
