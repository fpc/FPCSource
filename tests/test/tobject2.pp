

TYPE

  psimpleobject = ^tsimpleobject;
  tsimpleobject = object
   x: longint;
   z: array[0..34] of byte;
   Procedure Init(somez: longint);
   Procedure Hello;
  end;

  pbase = ^tbase;
  tbase = object
   numofentries : longint;
   constructor init(i : integer);
   destructor done; virtual;
   procedure showit; virtual;
  end;

  pderived = ^tderived;
  tderived = object(tbase)
   x: longint;
   constructor init;
   destructor done; virtual;
   procedure showit; virtual;
  end;


  Procedure TsimpleObject.init(somez: longint);
  var
     i: byte;
  Begin
     for i:=0 to 34 do
      z[i]:=i;
     x:=somez;
  end;


  Procedure TSimpleObject.hello;
  var
   i: byte;
  Begin
   WriteLn('hello world');
   for i:=0 to 34 do
     Write(z[i],' ');
     WriteLn;
     WriteLN(x);
  end;


  constructor tbase.init(i: integer);
  Begin
   numofentries := i;
  end;

  destructor tbase.done;
  Begin
  end;

  procedure tbase.showit;
  Begin
    WriteLn('This is the base class');
  end;

  constructor tderived.init;
  Begin
   inherited init(5);
   x:=10;
  end;

  procedure tderived.showit;
  Begin
   WriteLn('This is the derived class');
   WriteLn(numofentries);
   WriteLn(x);
  end;

  destructor tderived.done;
  Begin
  end;


  Procedure CreateObject;
  var
   obj: pbase;
  Begin
    obj^.showit;
    dispose(obj,done);
  end;

var
 myobj: tsimpleobject;
 obj: pbase;
 devobj: tderived;
Begin
 obj:=new(pbase,init(10));
 obj^.showit;
 dispose(obj,done);
end.
