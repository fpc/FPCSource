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
   saveexit : pointer;

  procedure testerror;
    begin
       exitproc:=saveexit;
       if errorcode=210 then
         begin
            errorcode:=0;
            writeln('Object valid VMT check works');
            runerror(0);
         end
       else
         halt(1);
    end;

begin
   saveexit:=exitproc;
   exitproc:=@testerror;
   fillchar(data,sizeof(data),12);
   p:=new(pa,init);
   p^.p;
   { the vmt pointer gets an invalid value: }
   pptrint(p)^:=ptrint(@data);
   { causes runerror }
   p^.p;
   halt(1);
end.
