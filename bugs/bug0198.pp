{$mode fpcobj}

type
   to1 = class (tobject)
       x : longint;
       constructor init;
       function GetCaps1 : Longint;virtual;abstract;
       function GetCaps2 : Longint;virtual;stdcall;
       function GetCaps : Longint;virtual;stdcall;abstract;
       destructor done;virtual; 
   end;
 
   constructor to1.init;
    begin
      x:=5;
    end;

   function to1.GetCaps2 : longint;
    begin
      GetCaps2:=x;
    end;
 
   destructor to1.done;
    begin
    end;

var o1 : to1;
begin
  o1:=to1.create;
  if o1.getCaps2<>5 then halt(1);
end.
