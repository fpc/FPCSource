{ %opt=-g-h }

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  sysutils;

function test1: longint; 
begin
   try
     try
       result:=1;
       raise exception.create('1');
     except
        exit(2); 
     end; 
   except
     exit(3);
   end;
end;

function test2: ansistring; 
begin
   result:='a';
   try
     try
       result:=result+'b';
       raise exception.create('2');
     except
        result:=result+'c'; 
     end; 
   except
     result:=result+'d';
   end;
end;

begin
  HaltOnNotReleased:=true;
  if test1<>2 then
    halt(1);
  if test2<>'abc' then
    halt(2)
end.
