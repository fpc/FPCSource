

{$mode objfpc}

type
   tbaseclass = class
     x : longint;
     class procedure virtual_class_method; virtual;
     class procedure call_virtual_class_method;
     class function get_type : pointer;
     function get_type2 : pointer;
     procedure check_type;
     class procedure class_check_type;
   end;

   tderivedclass = class(tbaseclass)
     y : longint;
     class procedure virtual_class_method; override;
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

procedure tbaseclass.virtual_class_method;

begin
  Writeln('Calling tbase class class method');
  tbasecalled:=true;
  if typeof(self)<>get_type then
    begin
      has_error:=true;
      Writeln('Error with typeof');
    end;
end;

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

procedure tbaseclass.class_check_type;
begin
  if typeof(self)<>get_type then
    begin
      Writeln('Compiler creates garbage');
      has_error:=true;
    end;
end;

class procedure tbaseclass.call_virtual_class_method;
begin
  virtual_class_method;
  if getsize<>sizeof(self) then
    begin
      Writeln('Compiler creates garbage');
      has_error:=true;
    end;
end;

procedure tderivedclass.virtual_class_method;

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

begin
 cb:=tbaseclass.create;
 cd:=tderivedclass.create;
 c1:=tbaseclass.create;

 basesize:=sizeof(cb);
 Writeln('Sizeof(cb)=',basesize);
 if basesize<>expected_size_for_tbaseclass then
   Writeln('not the expected size : ',expected_size_for_tbaseclass);

 derivedsize:=sizeof(cd);
 Writeln('Sizeof(ct)=',derivedsize);
 if derivedsize<>expected_size_for_tderivedclass then
   Writeln('not the expected size : ',expected_size_for_tderivedclass);

 cb.check_type;
 cd.check_type;
 cb.class_check_type;
 cd.class_check_type;
 tbaseclass.class_check_type;
 tderivedclass.class_check_type;

 tbaseclass.virtual_class_method;
 if not tbasecalled then
   has_error:=true;
 reset_booleans;

 tbaseclass.call_virtual_class_method;
 if not tbasecalled then
   has_error:=true;
 reset_booleans;

 tderivedclass.virtual_class_method;
 if not tderivedcalled then
   has_error:=true;
 reset_booleans;

 tderivedclass.call_virtual_class_method;
 if not tderivedcalled then
   has_error:=true;
 reset_booleans;

 c1.virtual_class_method;
 if not tbasecalled then
   has_error:=true;
 reset_booleans;

 c1.call_virtual_class_method;
 if not tbasecalled then
   has_error:=true;
 reset_booleans;

 c1.destroy;

 c1:=tderivedclass.create;

 c1.virtual_class_method;
 if not tderivedcalled then
   has_error:=true;
 reset_booleans;

 c1.call_virtual_class_method;
 if not tderivedcalled then
   has_error:=true;
 reset_booleans;

 c1.destroy;

 cc:=tbaseclass;
 cc.class_check_type;

 cc.virtual_class_method;
 if not tbasecalled then
   has_error:=true;
 reset_booleans;

 cc.call_virtual_class_method;
 if not tbasecalled then
   has_error:=true;
 reset_booleans;

 cc:=tderivedclass;

 cc.class_check_type;

 cc.virtual_class_method;
 if not tderivedcalled then
   has_error:=true;
 reset_booleans;

 cc.call_virtual_class_method;
 if not tderivedcalled then
   has_error:=true;
 reset_booleans;

 if has_error then
   begin
     Writeln('Error with class methods');
     halt(1);
   end;

end.
