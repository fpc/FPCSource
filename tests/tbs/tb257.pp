{ Old file: tbs0292.pp }
{ objects not finalized when disposed                  OK 0.99.13 (FK) }

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

const
  s : string = ' with suffix';
var
  refbefore : longint;
begin
  t:='test'+s;
  refbefore:=pansirec(pointer(t)-firstoff)^.ref;
  writeln('refcount before init: ',pansirec(pointer(t)-firstoff)^.ref);
  new(o,init(t));
  writeln('refcount after init: ',pansirec(pointer(t)-firstoff)^.ref);
  dispose(o,done);
  writeln('refcount after done: ',pansirec(pointer(t)-firstoff)^.ref);
  if refbefore<>pansirec(pointer(t)-firstoff)^.ref then
    Halt(1);
end.
