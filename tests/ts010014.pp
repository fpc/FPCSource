{$R+}
type
   ta = object
      constructor init;
      destructor done;
      procedure p;virtual;
   end;

   pa = ^ta;

constructor ta.init;

  begin
  end;

destructor ta.done;

  begin
  end;

procedure ta.p;

  begin
  end;

type
   plongint = ^longint;

var
   p : pa;
   data : array[0..4] of longint;

begin
   fillchar(data,sizeof(data),12);
   p:=new(pa,init);
   p^.p;
   { the vmt pointer gets an invalid value: }
   plongint(p)^:=longint(@data);
   { causes runerror }
   p^.p;
   halt(1);
end.
