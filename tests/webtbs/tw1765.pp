{$mode delphi}

type mytype=array[1..2] of string;
const myconst:mytype=('foo','bar');

procedure myproc(myparam:mytype);
begin
 writeln(myparam[1],' ',myparam[2]);
end;

begin
 myproc(myconst);
end.
