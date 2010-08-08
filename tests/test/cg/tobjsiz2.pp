
{ Variation without virtual classes : no VMT }
{ here sizeof directly returns a constant value }


type
   pbaseclass = ^tbaseclass;
   pderivedclass = ^tderivedclass;

   tbaseclass = object
     x : byte;
     function getsize : longint; static;
     procedure check_size;
   end;

   tderivedclass = object(tbaseclass)
     y : byte;
   end;

const
  expected_size_for_tbaseclass = sizeof(byte);
  expected_size_for_tderivedclass = 2*sizeof(byte);

var
  basesize : longint;
  derivedsize : longint;

function tbaseclass.getsize : longint;
begin
  { self = pointer to VMT }
  getsize:=sizeof(self);
end;

procedure tbaseclass.check_size;
begin
  if getsize<>sizeof(pointer) then
    begin
      Writeln('Compiler creates garbage ',sizeof(self),'<>',sizeof(pointer));
      halt(1);
    end;
end;


var
  cb : tbaseclass;
  cd : tderivedclass;
  c1 : pbaseclass;
begin
 new(c1);

 basesize:=sizeof(cb);
 Writeln('Sizeof(cb)=',basesize);
 if basesize<>expected_size_for_tbaseclass then
   begin
     Writeln('not the expected size : ',expected_size_for_tbaseclass);
     halt(1);
   end;

 derivedsize:=sizeof(cd);
 Writeln('Sizeof(ct)=',derivedsize);
 if derivedsize<>expected_size_for_tderivedclass then
   begin
     Writeln('not the expected size : ',expected_size_for_tderivedclass);
     halt(1);
   end;

  cb.check_size;
end.
