
{ Variation without virtual classes : no VMT }
{ here sizeof directly returns a constant value }


{$static on}

type
   pbaseclass = ^tbaseclass;
   pderivedclass = ^tderivedclass;

   tbaseclass = object
     x : longint;
     {constructor init;}
     function getsize : longint; static;
     function getsize2 : longint;
     procedure check_size; {virtual;}
     procedure static_check_size; static;
     procedure check_normal;
     procedure check_static; static;
     {procedure check_virtual; virtual;}
     {destructor done; virtual;}
   end;

   tderivedclass = object(tbaseclass)
     y : longint;
     procedure check_size; {virtual;}
   end;

const
  has_error : boolean = false;
  expected_size_for_tbaseclass = {sizeof(pointer) + }sizeof(longint);
  expected_size_for_tderivedclass = {sizeof(pointer) +} 2*sizeof(longint);

var
  basesize : longint;
  derivedsize : longint;

{constructor tbaseclass.init;
begin
end;

destructor tbaseclass.done;
begin
end;     }

function tbaseclass.getsize : longint;
begin
  getsize:=sizeof(self);
end;

function tbaseclass.getsize2 : longint;
begin
  getsize2:=sizeof(self);
end;

procedure tbaseclass.check_size;
begin
  if sizeof(self)<>getsize then
    begin
      Writeln('Compiler creates garbage');
      has_error:=true;
    end;
  if sizeof(self)<>getsize2 then
    begin
      Writeln('Compiler creates garbage');
      has_error:=true;
    end;
end;

procedure tbaseclass.static_check_size;
begin
  if sizeof(self)<>getsize then
    begin
      Writeln('Compiler creates garbage');
      has_error:=true;
    end;
end;

procedure tbaseclass.check_normal;
begin
  check_size;
  static_check_size;
end;

procedure tbaseclass.check_static;
begin
  {check_size;}
  static_check_size;
end;

{procedure tbaseclass.check_virtual;
begin
  check_size;
  static_check_size;
end;}


procedure tderivedclass.check_size;

begin
  Writeln('Calling tderived check_size method');
  inherited check_size;
end;

var
  cb : tbaseclass;
  cd : tderivedclass;
  c1 : pbaseclass;
begin
 {cb.init;
 cd.init;}
 new(c1);

 basesize:=sizeof(cb);
 Writeln('Sizeof(cb)=',basesize);
 if basesize<>expected_size_for_tbaseclass then
   Writeln('not the expected size : ',expected_size_for_tbaseclass);

 derivedsize:=sizeof(cd);
 Writeln('Sizeof(ct)=',derivedsize);
 if derivedsize<>expected_size_for_tderivedclass then
   Writeln('not the expected size : ',expected_size_for_tderivedclass);

 cb.check_size;
 cd.check_size;
 c1^.check_size;
 cb.static_check_size;
 cd.static_check_size;
 c1^.static_check_size;
 tbaseclass.static_check_size;
 tderivedclass.static_check_size;
 tbaseclass.check_static;
 tderivedclass.check_static;

 cb.check_normal;
 cb.check_static;
 cd.check_normal;
 cd.check_static;

 if has_error then
   begin
     Writeln('Error with object methods');
     halt(1);
   end;

end.
