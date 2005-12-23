{$mode objfpc}{$H+}

uses
   Classes, SysUtils, Variants;

var
  err : boolean;

function IsVariantOfType( pVariant : Variant ; pVarType : TVarType ) :
boolean ;
var
   xVT : TVarType;
   xVTHigh : TVarType;
//  xVTLow : TVarType;
begin
//  result := ( varType( pVariant ) and pVarType ) = pVarType ;
// Contr: VarType is varDate = 0007, pVarType is varInteger=0003.
// 0007 and 0003 = 0003. WRONG!

   xVT := VarType(pVariant);
//  xVTLow:=xVT and varTypeMask;
   xVTHigh := xVT and (not varTypeMask);

   // in true pVarType can be and OR of two types: varArray and varString (or others)
   // we have to recognize it.
   // there shouldn't be xVTLow because when we have array of string (normal) then
   // xVT=$2008 = $2000 (var Array) or $0008 (var String)
   // then when we asked:
   //   is $2000 (varArray)? we should receive TRUE (xVTHigh=pVarType)
   //   is $2008 (varArray of varString)? we should receive TRUE (xVT=pVarType)
   //   is $0008 (varString)? we should receive FALSE
   Result := (xVT=pVarType) or ((xVTHigh=pVarType) and (xVTHigh<>varEmpty));
end ;

procedure TestIsVariantOfType ;

   procedure _tiIsVariantOfType(xVar : variant; xExpected : TVarType; xMsg : string);

     procedure __tiIsVariantOfType(xxCheck : TVarType; xxMsg : string);
     begin
       if xxCheck=xExpected then
       begin
         If not IsVariantOfType( xVar, xxCheck ) then
           begin
             Writeln(xMsg);
             err:=true;
           end;
       end
       else
       begin
         If IsVariantOfType( xVar, xxCheck ) then
           begin
             Writeln(xMsg + ' - ' + xxMsg);
             err:=true;
           end;
       end;
     end;

   begin
     __tiIsVariantOfType(varEmpty,'varEmpty');
     __tiIsVariantOfType(varNull,'varNull');
     __tiIsVariantOfType(varSmallint,'varSmallInt');
     __tiIsVariantOfType(varInteger,'varInteger');
     __tiIsVariantOfType(varSingle,'varSingle');
     __tiIsVariantOfType(varDouble,'varDouble');
     __tiIsVariantOfType(varDate,'varDate');
     __tiIsVariantOfType(varBoolean,'varBoolean');
     __tiIsVariantOfType(varOleStr,'varOleStr');
   end;
var
   lVar : Variant ;
   lSmallInt : Smallint;
   lInteger : Integer;
   lDouble : Double;
   lDateTimeNow : TDateTime;
   lDateTimeDate : TDateTime;
   lOleString : WideString;
   lString : string;
   lBoolean : boolean;
   lCurrency : Currency;
begin
   lSmallInt := 123;
   lInteger := High(Integer);
   lDouble := 123.45678901234567890;
   lDateTimeNow := Now;
   lDateTimeDate := Date;
   lOleString := 'OLE STRING TEST';
   lString := 'STRING TEST';
   lBoolean := true;
   lCurrency := 12345678.9876;

   lVar := Unassigned;
   _tiIsVariantOfType(lVar,varEmpty,'Failed with varEmpty');

   lVar := Null ;
   _tiIsVariantOfType(lVar,varNull,'Failed with varNull');

   // There is no other way to receive variant of type small int...
   lVar:=VarAsType(lSmallInt,varSmallint);
   _tiIsVariantOfType(lVar,varSmallInt,'Failed with VarSmallint');

   lVar:=lInteger;
   _tiIsVariantOfType(lVar,varInteger,'Failed with Integer');

// Can't make this one work
   lVar:=VarAsType(123.456,varSingle);
   _tiIsVariantOfType(lVar,varSingle,'Failed with VarSingle');

   lVar:=lDouble;
   _tiIsVariantOfType(lVar,varDouble,'Failed with VarDouble');

   lVar:=lDateTimeDate;
   _tiIsVariantOfType(lVar,varDate,'Failed with varDate - DATE');

   lVar:=lDateTimeNow;
   _tiIsVariantOfType(lVar,varDate,'Failed with varDate - NOW');

   lVar:=lBoolean;
   _tiIsVariantOfType(lVar,varBoolean,'Failed with varBoolean');

   lVar:=lOleString;
   _tiIsVariantOfType(lVar,varOLEStr,'Failed with varOLEStr');

   lVar := lString;
   _tiIsVariantOfType(lVar, varString, 'Failed with varString');

   lVar:=lCurrency;
   _tiIsVariantOfType(lVar,varCurrency,'Failed with varCurrency');

// These ones have not been tested
// varCurrency        Currency floating-point value (type Currency).
// varDispatch        Reference to an Automation object (an IDispatch interface pointer).
// varError        Operating system error code.
// varUnknown        Reference to an unknown COM object (an IUnknown interface pointer).
// varByte        8-bit unsigned integer (type Byte).
// varTypeMask        Bit mask for extracting type code.
// varArray        Bit indicating variant array.
// varByRef        Bit indicating variant contains a reference (rather than a value).
end;


begin
   TestIsVariantOfType;
   if err then
     halt(1);
end.
