{$mode objfpc}
Program TestAOC;

{ Program to test array of const }

{ All elements of the following record must be tested :
  Elements not yet tested are commented out.

    Type
       PVarRec = ^TVarRec;
       TVarRec = record
         case vType: Byte of
           vtInteger    : (VInteger: Integer; VType:Longint);
           vtBoolean    : (VBoolean: Boolean);
           vtChar       : (VChar: Char);
           vtExtended   : (VExtended: PExtended);
           vtString     : (VString: PShortString);
           vtPointer    : (VPointer: Pointer);
           vtPChar      : (VPChar: PChar);
           vtObject     : (VObject: TObject);
           vtClass      : (VClass: TClass);
           // vtWideChar   : (VWideChar: WideChar);
           // vtPWideChar  : (VPWideChar: PWideChar);
           vtAnsiString : (VAnsiString: Pointer);
           // vtCurrency   : (VCurrency: PCurrency);
           // vtVariant    : (VVariant: PVariant);
           // vtInterface  : (VInterface: Pointer);
           // vtWideString : (VWideString: Pointer);
           vtInt64      : (VInt64: PInt64);
           vtQWord      : (VQWord: PQWord);
       end;
}

procedure testit2 (args: array of byte);
begin
end;

Procedure Testit (Args: Array of const);

Var I : longint;

begin
  If High(Args)<0 then
    begin
    Writeln ('No aguments');
    exit;
    end;
  Writeln ('Got ',High(Args)+1,' arguments :');
  For i:=0 to High(Args) do
    begin
    write ('Argument ',i,' has type ');
    case Args[i].vtype of
      vtinteger    : Writeln ('Integer, Value :',args[i].vinteger);
      vtboolean    : Writeln ('Boolean, Value :',args[i].vboolean);
      vtchar       : Writeln ('Char, value : ',args[i].vchar);
      vtextended   : Writeln ('Extended, value : ',args[i].VExtended^);
      vtString     : Writeln ('ShortString, value :',args[i].VString^);
      vtPointer    : Writeln ('Pointer, value : ',Longint(Args[i].VPointer));
      vtPChar      : Writeln ('PCHar, value : ',Args[i].VPChar);
      vtObject     : Writeln ('Object, name : ',Args[i].VObject.Classname);
      vtClass      : Writeln ('Class reference, name : ',Args[i].VClass.Classname);
      vtAnsiString : Writeln ('AnsiString, value :',AnsiString(Args[I].VAnsiString));

{
      vtWideChar   : (VWideChar: WideChar);
      vtPWideChar  : (VPWideChar: PWideChar);
      vtCurrency   : (VCurrency: PCurrency);
      vtVariant    : (VVariant: PVariant);
      vtInterface  : (VInterface: Pointer);
      vtWideString : (VWideString: Pointer);
}
      vtInt64      : Writeln ('Int64, value : ',args[i].VInt64^);
      vtQWord      : Writeln ('QWord, value : ',args[i].VQWord^);
    else
      Writeln ('(Unknown) : ',args[i].vtype);
    end;
    end;
end;

Const P1 : Pchar = 'Eerste Pchar';
      p2 : Pchar = 'Tweede pchar';

Var ObjA,ObjB : TObject;
    ACLass,BClass : TClass;
    S,T : AnsiString;

begin
  ObjA:=TObject.Create;
  ObjB:=TObject.Create;
  AClass:=TObject;
  S:='Ansistring 1';
  T:='AnsiString 2';
  writeln ('Size of VarRec : ',Sizeof(TVarRec));
  Testit ([]);
  Testit ([1,2]);
  Testit (['A','B']);
  Testit ([TRUE,FALSE,TRUE]);
  Testit (['String','Another string']);
  Testit ([S,T])  ;
  Testit ([P1,P2]);
  Testit ([@testit,Nil]);
  Testit ([ObjA,ObjB]);
  Testit ([1.234,1.234]);
  TestIt ([AClass]);
  TestIt ([QWord(1234)]);
  TestIt ([Int64(1234)]);
  TestIt ([Int64(12341234)*1000000000+Int64(12341234)]);

  TestIt2 ([]);
  TestIt2 ([1,2]);
end.
