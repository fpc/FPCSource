
type
stlpos=^stlink;
stlink=
  packed object
   next,prev:stlpos;
   data:string;
   procedure save(var f:file);
   procedure load(var f:file);
  end;
list_of_string=
  packed object
   parent:pointer;
   anchor:stlpos;
   last:stlpos;
   len:longint;
   constructor init(p:pointer);
   function first:stlpos;
   function next(p:stlpos):stlpos;
   function prev(p:stlpos):stlpos;
   function ret(p:stlpos):string;
   function pret(p:stlpos):pointer;
   function insert(p:stlpos;d:string):stlpos;
   function remove(p:stlpos):stlpos;
   function empty:boolean;
   destructor term;
   procedure save(var f:file);
   procedure load(var f:file);
  end;

stack_of_string=
  packed object (list_of_string)
   procedure pop(var s:string);
   procedure push(const s:string);
  end;

procedure writestring(var f:file;s:string);
begin
  blockwrite(f,s,length(s)+1);
end;

procedure readstring(var f:file;var s:string);
var b:byte;
begin
  blockread(f,b,1);
  seek(f,filepos(f)-1);
  blockread(f,s,b+1);
end;

constructor list_of_string.init(p:pointer);
begin
  parent:=p;
  new(anchor);
  anchor^.next:=nil;
  anchor^.prev:=nil;
  last:=anchor;
  len:=0;
end;

function list_of_string.first:stlpos;
begin
  first:=anchor^.next;
end;

function list_of_string.next(p:stlpos):stlpos;
begin
  next:=p^.next;
end;

function list_of_string.prev(p:stlpos):stlpos;
begin
  prev:=p^.prev;
end;

function list_of_string.ret(p:stlpos):string;
begin
  ret:=p^.data;
end;

function list_of_string.pret(p:stlpos):pointer;
begin
  pret:=@(p^.data);
end;

function list_of_string.insert(p:stlpos;d:string):stlpos;
var t:stlpos;
begin
  new(t);
  t^.prev:=p;
  t^.next:=p^.next;
  p^.next:=t;
  if t^.next<>nil then
   t^.next^.prev:=t
  else
   last:=t;
  t^.data:=d;
  inc(len);
  insert:=t;
end;

function list_of_string.remove(p:stlpos):stlpos;
begin
  if p^.prev<>nil then
   p^.prev^.next:=p^.next;
  if p^.next<>nil then
   p^.next^.prev:=p^.prev
  else
   last:=p^.prev;
  dispose(p);
  dec(len);
  remove:=p^.next;
end;

function list_of_string.empty:boolean;
begin
  empty:=(last=anchor);
end;

destructor list_of_string.term;
begin
  while not empty do
   remove(last);
  dispose(anchor);
  last:=nil;
end;

procedure stlink.save(var f:file);
begin
  writestring(f,data);
end;

procedure list_of_string.save(var f:file);
var
  l,i:longint;
  p:stlpos;
begin
  l:=len;
  blockwrite(f,l,sizeof(longint));
  p:=first;
  for i:=1 to l do
   begin
    p^.save(f);
    p:=next(p);
   end;
end;

procedure stlink.load(var f:file);
begin
  readstring(f,data);
end;

procedure list_of_string.load(var f:file);
var
  l,i:longint;
  d:stlink;
begin
  blockread(f,l,sizeof(longint));
  for i:=1 to l do
   begin
    d.load(f);
    insert(last,d.data);
   end;
end;

procedure stack_of_string.pop(var s:string);
begin
  if not empty then
   begin
    s:=first^.data;
    remove(first);
   end;
end;

procedure stack_of_string.push(const s:string);
begin
  insert(anchor,s);
end;

var
  gs:stack_of_string;
  opaddr : ^string;
begin
  gs.init(nil);
  gs.push('test');

  {perfectly compiles}
  opaddr:=@((gs.first)^.data);
  writeln(opaddr^);
  if (opaddr^<>'test') then
   halt(1);

  {reports error ") expected ^ but found"}
  opaddr:=@(gs.first^.data);
  writeln(opaddr^);
  if (opaddr^<>'test') then
   halt(1);


end.
