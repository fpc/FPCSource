{%OPT=-CR}

{ This test checks that correct code is generated
  when typecasting a class reference type variable with a descendent class }

{$mode objfpc}

uses
  sysutils;

type

  TBaseClass = class
   constructor Create;
   class var  x : longint;
   var loc : longint;
   class procedure check; virtual;
  end;

  TDerClass = class(TBaseClass)
   var der : longint;
  end;

  TDer1Class = class(TDerClass)
   constructor Create;
   class var y : longint;
   var loc1 : longint;
   class procedure check; override;
  end;

  TDer2Class = class(TDerClass)
   constructor Create;
   class var z : longint;
   var loc2 : longint;
   class procedure check; override;
  end;

constructor TBaseClass.Create;
  begin
    Inherited Create;
    x:=1;
  end;

constructor TDer1Class.Create;
  begin
    Inherited Create;
    y:=1;
  end;

constructor TDer2Class.Create;
  begin
    Inherited Create;
    z:=1;
  end;

class procedure TBaseClass.check;
begin
  writeln('TBaseClass.check called');
end;

class procedure TDer1Class.check;
begin
  writeln('TDer1Class.check called');
end;

class procedure TDer2Class.check;
begin
  writeln('TDer2Class.check called');
end;

type
  TBaseClassRef = class of TBaseClass;
  TDerClassRef = class of TDerClass;

var
  c : TBaseClass;
  cc : TBaseClassRef;
  dcc : TDerClassRef;
  exception_generated : boolean;

begin
  exception_generated:=false;
  c:=TBaseClass.Create;

  inc(c.x);
  c.check;
  c.free;

  c:=TDer1Class.Create;

  inc(c.x);
  inc(TDer1Class(c).y);
  c.check;
  c.free;

  c:=TDer2Class.Create;
  inc(c.x);
  inc(TDer2Class(c).z);
  c.check;
  c.free;

  cc:=TbaseClass;
  inc(cc.x);
  cc.check;

  cc:=TDer1Class;
  inc(cc.x);
  cc.check;


  cc:=TDer2Class;
  inc(cc.x);
  cc.check;
  TDerClassRef(cc).check;
  TDerClass(cc).check;

  dcc:=TDerClass;
  dcc.check;

  try
    //inc (TDer1Class(cc).y);
    TDer1Class(cc).check;
  except
    writeln('Exception generated');
    exception_generated:=true;
  end;
  writeln('TBaseClass: x=',TBaseClass.x);
  writeln('TDer1Class: x=',TDer1Class.x,', y=',TDer1Class.y);
  writeln('TDer2Class: x=',TDer2Class.x,', z=',TDer2Class.z);
  if not exception_generated then
    begin
      writeln('No exception generated on wrong typecast of class reference variable');
      halt(1);
    end;
end.

