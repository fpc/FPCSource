{$mode objfpc}

{unit ghashset;

interface}
uses gvector;

const baseSize = 8;



{Thash should have one class function hash(a:T, n:longint):longint which return uniformly distributed
value in range <0,n-1> base only on arguments}

type 
  generic hashset<T, Thash>=class
    private type TContainer = specialize vector<T>;
    type TTable = specialize vector<TContainer>;
    var data:TTable;
    public constructor create;
    procedure insert(value:T);inline;
    function find(value:T):boolean;inline;
  end;

{implementation}

constructor hashset.create;
var i:longint;
begin
  data:=TTable.create;
  data.resize(8);
  for i:=0 to 7 do
    data[i]:=TContainer.create;
end;

function hashset.find(value:T):boolean;inline;
var i,h,bs:longint;
begin
  h:=Thash.hash(value,data.size);
  bs:=data.getValue(h).size;
  for i:=0 to bs-1 do begin
    if (data.getvalue(h).getvalue(i)=value) then exit(true);
  end;
  exit(false);
end;

procedure hashset.insert(value:T);inline;
begin
  if (find(value)) then exit;
  (data[Thash.hash(value,data.size)]).pushback(value);
end;

type hint=class
  class function hash(a,n:longint):longint;
end;

class function hint.hash(a,n:longint):longint;
begin
  hash:= a mod n;
end;

type hsli = specialize hashset<longint, hint>;

var data:hsli; i,n:longint;

begin
  data:=hsli.create;
  for i:=0 to 10 do
    data.insert(i);
  for i:=0 to 13 do
    writeln(data.find(i));
end.
