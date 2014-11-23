program fpc_advrec_bug;

{$mode delphi}
{$optimization off}

Uses TypInfo;

Type

 PTypeInfoRec = Record
  FValue : PTypeInfo;
  Function QualifiedName : String;
 End;

Function PTypeInfoRec.QualifiedName : String;
Begin
 Result := '';
End;

function Test : Pointer;
Begin
 Result := nil;
End;

Var

 p : PTypeInfo;

begin

 PTypeInfoRec(p).QualifiedName; // OK
 PTypeInfoRec(Test).QualifiedName; // OK
 PTypeInfoRec(TypeInfo(String)).QualifiedName; // Internal error 200304235

end.
