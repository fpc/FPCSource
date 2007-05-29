{ Source provided for Free Pascal Bug Report 4634 }
{ Submitted by "Graeme Geldenhuys" on  2005-12-23 }
{ e-mail: graemeg@gmail.com }
program Project1;
{$ifdef fpc}
{$mode objfpc}
{$endif fpc}
{$H+}
uses
  Classes, SysUtils, Variants;

function IsVariantOfType( pVariant : Variant ; pVarType : TVarType ) : boolean ;
var
  xVT : TVarType;
  xVTHigh : TVarType;
begin
//  result := ( varType( pVariant ) and pVarType ) = pVarType ;
// Contr: VarType is varDate = 0007, pVarType is varInteger=0003.
// 0007 and 0003 = 0003. WRONG!

  xVT := VarType(pVariant);
  xVTHigh := xVT and (not varTypeMask);

{  in true pVarType can be and OR of two types: varArray and varString (or others)
   we have to recognize it.
   there shouldn't be xVTLow because when we have array of string (normal) then
   xVT=$2008 = $2000 (var Array) or $0008 (var String)
   then when we asked:
     is $2000 (varArray)? we should receive TRUE (xVTHigh=pVarType)
     is $2008 (varArray of varString)? we should receive TRUE (xVT=pVarType)
     is $0008 (varString)? we should receive FALSE
}
  Result := (xVT=pVarType) or ((xVTHigh=pVarType) and (xVTHigh<>varEmpty));
end ;

procedure TestIsVariantOfType ;

  procedure _tiIsVariantOfType(xVar : variant; xExpected : TVarType; xMsg : string);

    procedure __tiIsVariantOfType(xxCheck : TVarType; xxMsg : string);
    begin
      if xxCheck=xExpected then
      begin
        If not IsVariantOfType( xVar, xxCheck ) then
          Writeln(xMsg);
      end
      else
      begin
        If IsVariantOfType( xVar, xxCheck ) then
          Writeln(xMsg + ' - ' + xxMsg);
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
  lDouble := 123.45678901234567890;

// Can't make this one work
  lVar:=VarAsType(123.456,varSingle);
  _tiIsVariantOfType(lVar,varSingle,'Failed with VarSingle');

  lVar:=lDouble;
  _tiIsVariantOfType(lVar,varDouble,'Failed with VarDouble');
end;

begin
  TestIsVariantOfType;
end.
