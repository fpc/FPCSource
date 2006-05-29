

{$mode objfpc}

type
   tbaseclass = class
     x : longint;
     class procedure virtual_class_method; virtual;
     class procedure call_virtual_class_method;
   end;

   tderivedclass = class(tbaseclass)
     y : longint;
     class procedure virtual_class_method; override;
   end;

const
  tbasecalled : boolean = false;
  tderivedcalled : boolean = false;
  has_error : boolean = false;
class procedure tbaseclass.virtual_class_method;

begin
  Writeln('Calling tbase class class method');
  tbasecalled:=true;
end;

class procedure tbaseclass.call_virtual_class_method;
begin
  virtual_class_method;
end;

class procedure tderivedclass.virtual_class_method;

begin
  Writeln('Calling tderived class class method');
  tderivedcalled:=true;
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

 cc.virtual_class_method;
 if not tbasecalled then
   has_error:=true;
 reset_booleans;

 cc.call_virtual_class_method;
 if not tbasecalled then
   has_error:=true;
 reset_booleans;

 cc:=tderivedclass;

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
