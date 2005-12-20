{$mode objfpc}{$H+}

uses
   Classes, SysUtils, TypInfo;


type
{$M+}
   TMyTestObject = class;
{$M-}


   TSomeOtherClass = class(TObject)
   private
      FName: string;
   public
     property Name: string read FName write FName;
   end;


   TMyTestObject = class(TObject)
   private
      FIntProp: integer;
      FStringProp: string;
   public
   published
     property StringProp: string read FStringProp write FStringProp;
     property IntProp: integer read FIntProp write FIntProp;
   end;


procedure ShowProperties;
var
   O: TMyTestObject;
   i: Longint;
   lPropFilter: TTypeKinds;
   lCount: Longint;
   lSize: Integer;
   lList: PPropList;
begin
   O := TMyTestObject.Create;
   lPropFilter := [tkInteger, tkAString];

   lCount  := GetPropList(O.ClassInfo, lPropFilter, nil, false);
   lSize   := lCount * SizeOf(Pointer);
   GetMem(lList, lSize);

   Writeln('Total property Count: ' + IntToStr(lCount));
   lCount := GetPropList(O.ClassInfo, lPropFilter, lList, false);
   for i := 0 to lCount-1 do
   begin
     Writeln('Property '+IntToStr(i+1)+': ' + lList^[i]^.Name);
   end;

   FreeMem(lList);
   O.Free;
   Writeln('---------------');
end;


begin
   ShowProperties;
end.
