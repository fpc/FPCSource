{$mode objfpc}

type
  pobj = ^tobj;
  tobj = object
    a: ansistring;
    constructor init(s: ansistring);
    destructor done;
  end;

  PAnsiRec = ^TAnsiRec;
  TAnsiRec = Packed Record
    Maxlen,
    len,
    ref   : Longint;
    First : Char;
  end;

const firstoff = sizeof(tansirec)-1;

var o: pobj;
    t: ansistring;

constructor tobj.init(s: ansistring);
begin
  a := s;
end;

destructor tobj.done;
begin
end;

begin
  readln(t);
  writeln('refcount before init: ',pansirec(pointer(t)-firstoff)^.ref);
  new(o,init(t));
  writeln('refcount after init: ',pansirec(pointer(t)-firstoff)^.ref);
  dispose(o,done);
  writeln('refcount after done: ',pansirec(pointer(t)-firstoff)^.ref);
end.