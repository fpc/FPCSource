program bug;

{$Q+}

const s:array[0..31] of char='Hell* world';
	  index:longint=-6;

var c:char;
    p,q:Pchar;

begin 
  p:=s;
  q:=p-index;
  writeln('Hello ',q);
end.
