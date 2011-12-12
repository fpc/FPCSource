program tpvar;

{$mode objfpc}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
  tmprec = record
    b: byte;
  end;

  tmethodclass = class
    l: longint;
    procedure test(x: longint; w: word; r: tmprec; var ro: tmprec);
    class procedure classproc(b: longint);
    class procedure callclassproc;
  end;

  tmethodclass2 = class(tmethodclass)
    class procedure classproc(b: longint);
  end;

  tmypvar = procedure(x: longint; w: word; r: tmprec; var ro: tmprec) of object;


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


  class procedure tmethodclass.classproc(b: longint);
    begin
      jlsystem.fout.println('tmethodclass.classproc');
    end;

  class procedure tmethodclass.callclassproc;
    type
      pv = procedure(l: longint) of object;
    var
      v: pv;
    begin
      v:=@classproc;
      v(3);
    end;



  class procedure tmethodclass2.classproc(b: longint);
    begin
      jlsystem.fout.println('tmethodclass2.classproc');
    end;

type
  tcc = class of tmethodclass;

var
  mypvar: tmypvar;
  c: tmethodclass;
  r, ro: tmprec;
  cc: tcc;
begin
  r.b:=21;
  ro.b:=42;
  c:=tmethodclass2.create;
  c.l:=6;
  mypvar:=@c.test;
  mypvar(1,$ffff,r,ro);
  if r.b<>21 then
    raise jlexception.create('r changed');
  if ro.b<>123 then
    raise jlexception.create('ro not changed');
  c.free;

  tmethodclass.callclassproc;
  tmethodclass2.callclassproc;
  cc:=tmethodclass;
  cc.callclassproc;
  cc:=tmethodclass2;
  cc.callclassproc;
end.
