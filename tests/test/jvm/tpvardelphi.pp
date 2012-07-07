program tpvardelphi;

{$mode delphi}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
  tmprec = record
    b: byte;
  end;

  tmethodclass = class
    l: longint;
    procedure test(x: longint; w: word; r: tmprec; var ro: tmprec);
    procedure shorttest(b: byte);
    procedure shorttest2(b: byte);
  end;

  tmypvar = procedure(x: longint; w: word; r: tmprec; var ro: tmprec) of object;
  tmyshortpvar = procedure(b: byte) of object;


  procedure tmethodclass.test(x: longint; w: word; r: tmprec; var ro: tmprec);
    begin
      jlsystem.fout.print('l: ');
      jlsystem.fout.println(l);
      jlsystem.fout.print('x: ');
      jlsystem.fout.println(x);
      jlsystem.fout.print('w: ');
      jlsystem.fout.println(w);
      jlsystem.fout.print('r.b: ');
      jlsystem.fout.println(r.b);
      jlsystem.fout.print('ro.b: ');
      jlsystem.fout.println(ro.b);
      if l<>6 then
        raise jlexception.create('l wrong on input');
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
    end;

  procedure tmethodclass.shorttest(b: byte);
    begin
      if b<>129 then
        raise jlexception.create('shorttest b wrong');
      if l<>7 then
        raise jlexception.create('shorttest l wrong');
    end;

  procedure tmethodclass.shorttest2(b: byte);
    begin
      if b<>130 then
        raise jlexception.create('shorttest2 b wrong');
      if l<>6 then
        raise jlexception.create('shorttest l wrong');
    end;

var
  mypvar, mypvar2: tmypvar;
  c,c2: tmethodclass;
  r, ro: tmprec;
  meth: tmethod;
  shortpvar1,shortpvar2: tmyshortpvar;
begin
  r.b:=21;
  ro.b:=42;
  c:=tmethodclass.create;
  c.l:=6;
  mypvar:=c.test;
  meth:=tmethod(mypvar);
  mypvar:=tmypvar(meth);
  mypvar(1,$ffff,r,ro);
  if r.b<>21 then
    raise jlexception.create('r changed');
  if ro.b<>123 then
    raise jlexception.create('ro not changed');

  c2:=tmethodclass.create;
  c2.l:=7;

  shortpvar1:=c.shorttest;
  shortpvar2:=c2.shorttest2;
  { should only copy the procedure pointer, not the instance ->
    instance.l=6, expected parameter = 130 }
  @shortpvar1:=@shortpvar2;
  shortpvar1(130);

  c.free;
end.
