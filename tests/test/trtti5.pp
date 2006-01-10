{$IFDEF FPC}
   {$mode objfpc}{$H+}
{$ELSE}
   {$APPTYPE CONSOLE}
{$ENDIF}

uses
   SysUtils,
   TypInfo,
   Classes;

type
   TAObject = class(TPersistent)
   private
     FIntProp: Integer;
   published
     property IntProp: Integer read FIntProp write FIntProp;
   end;

   TBObject = class(TAObject)
   published
     property IntProp default 1;
   end;


   TCObject = class(TBObject)
   published
     property IntProp default 2;
   end;

procedure ShowProperties;
var
   Obj: TCObject;
   i: Longint;
   lPropFilter: TTypeKinds;
   lCount: Longint;
   lSize: Integer;
   lList: PPropList;
begin
   Obj := TCObject.Create;
   lPropFilter := [tkInteger, tkLString {$ifdef FPC}, tkAString{$endif}];

   lCount  := GetPropList(Obj.ClassInfo, lPropFilter, nil, false);
   lSize   := lCount * SizeOf(Pointer);
   GetMem(lList, lSize);

   Writeln('Total property Count: ' + IntToStr(lCount));
   lCount := GetPropList(Obj.ClassInfo, lPropFilter, lList, false);
   for i := 0 to lCount-1 do
   begin
     Writeln('Property '+IntToStr(i+1)+': ' + lList^[i]^.Name);
   end;

   if lCount<>1 then
     halt(1);

   FreeMem(lList);
   Obj.Free;
   Writeln('---------------');
end;


begin
   ShowProperties;
end.
