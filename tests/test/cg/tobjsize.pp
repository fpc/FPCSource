type
   pbaseclass = ^tbaseclass;
   pderivedclass = ^tderivedclass;

   tbaseclass = object
     x : longint;
     constructor init;
     destructor done; virtual;
   end;

   tderivedclass = object(tbaseclass)
     y : longint;
   end;

const
  has_error : boolean = false;
  expected_size_for_tbaseclass = sizeof(pointer) + sizeof(longint);
  expected_size_for_tderivedclass = sizeof(pointer) + 2*sizeof(longint);

var
  basesize : longint;
  derivedsize : longint;

constructor tbaseclass.init;
begin
end;

destructor tbaseclass.done;
begin
end;

var
  cb : tbaseclass;
  cd : tderivedclass;
begin
 cb.init;
 cd.init;

 basesize:=sizeof(cb);
 Writeln('Sizeof(cb)=',basesize);
 if basesize<>expected_size_for_tbaseclass then
   Writeln('not the expected size : ',expected_size_for_tbaseclass);

 derivedsize:=sizeof(cd);
 Writeln('Sizeof(ct)=',derivedsize);
 if derivedsize<>expected_size_for_tderivedclass then
   Writeln('not the expected size : ',expected_size_for_tderivedclass);

end.
