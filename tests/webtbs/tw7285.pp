{ %opt=-Sehw -S2 }

type myclass=class(TObject)
      procedure myproc(); 
     end; 

procedure myclass.myproc(); 
var avalue:integer=100; 
begin
     writeln(avalue); 
end; 


var
  c: myclass;
begin
  c := myclass.create;
  c.myproc;
  c.free;
end.
