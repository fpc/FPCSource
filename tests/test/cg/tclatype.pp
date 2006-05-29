

{$mode objfpc}

type
   tbaseclass = class
     x : longint;
     function get_type : pointer;
     function get_type2 : pointer;virtual;
     procedure check_type;
     class procedure virtual_class_method;virtual;
   end;

   tderivedclass = class(tbaseclass)
     y : longint;
     function get_type2 : pointer;override;
     class procedure virtual_class_method;override;
   end;

const
  tbasecalled : boolean = false;
  tderivedcalled : boolean = false;
  has_error : boolean = false;
  expected_size_for_tbaseclass = sizeof(pointer) + sizeof(longint);
  expected_size_for_tderivedclass = sizeof(pointer) + 2*sizeof(longint);

var
  basesize : longint;
  derivedsize : longint;

function tbaseclass.get_type : pointer;
begin
  get_type:=typeof(self);
end;

function tbaseclass.get_type2 : pointer;
begin
  get_type2:=typeof(self);
end;

procedure tbaseclass.check_type;
begin
  if typeof(self)<>get_type then
    begin
      Writeln('Compiler creates garbage');
      has_error:=true;
    end;
  if typeof(self)<>get_type2 then
    begin
      Writeln('Compiler creates garbage');
      has_error:=true;
    end;
  if get_type<>get_type2 then
    begin
      Writeln('get_type and get_type2 return different pointers');
      has_error:=true;
    end;
end;

class procedure tbaseclass.virtual_class_method;
begin
  Writeln('Calling tbase class class method');
  tbasecalled:=true;
  if sizeof(self)<>basesize then
    begin
      has_error:=true;
      Writeln('Error with sizeof');
    end;
end;

function tderivedclass.get_type2 : pointer;
begin
  get_type2:=typeof(self);
end;

class procedure tderivedclass.virtual_class_method;
begin
  Writeln('Calling tderived class class method');
  tderivedcalled:=true;
  if sizeof(self)<>derivedsize then
    begin
      has_error:=true;
      Writeln('Error with sizeof');
    end;
end;

procedure reset_booleans;
begin
  tbasecalled:=false;
  tderivedcalled:=false;
end;

var
  c1,cb : tbaseclass;
  cd : tderivedclass;
  cc : class of tbaseclass;
  pb,pd : pointer;

begin
 cb:=tbaseclass.create;
 cd:=tderivedclass.create;
 c1:=tbaseclass.create;

 basesize:=sizeof(cb);
 Writeln('Sizeof(cb)=',basesize);
 if basesize<>sizeof(pointer) then
   Writeln('not the expected size : ',sizeof(pointer));

 derivedsize:=sizeof(cd);
 Writeln('Sizeof(ct)=',derivedsize);
 if derivedsize<>sizeof(pointer) then
   Writeln('not the expected size : ',sizeof(pointer));

 cb.check_type;
 cd.check_type;

 c1.destroy;

 c1:=tderivedclass.create;

 c1.virtual_class_method;
 if not tderivedcalled then
   has_error:=true;
 reset_booleans;

 c1.destroy;

 cc:=tbaseclass;

 cc.virtual_class_method;
 if not tbasecalled then
   has_error:=true;
 reset_booleans;

 cc:=tderivedclass;


 cc.virtual_class_method;
 if not tderivedcalled then
   has_error:=true;
 reset_booleans;

 if has_error then
   begin
     Writeln('Error with class methods');
     halt(1);
   end;

end.
