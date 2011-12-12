{$mode objfpc}
Program tarray2;

{$ifdef cpujvm}
uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define writeln:=jlsystem.fout.println}
{$define write:=jlsystem.fout.print}

{$else}
uses
  SysUtils;
{$endif}

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
  Write ('Got '); Write (High(Args)+1); Writeln(' arguments :');
  For i:=0 to High(Args) do
    begin
    write ('Argument '); write(i); write(' has type ');
    case Args[i].vtype of
      vtinteger    : begin Write ('Integer, Value :'); Writeln(args[i].vinteger); end;
      vtboolean    : begin Write ('Boolean, Value :'); Writeln(args[i].vboolean); end;
      vtchar       : begin Write ('Char, value : '); Writeln(args[i].vchar); end;
      vtextended   : begin Write ('Extended, value : '); Writeln(args[i].VExtended^); end;
      vtString     : begin Write ('ShortString, value :'); Writeln(unicodestring(args[i].VString^)); end;
      vtPointer    : begin Write ('Pointer, toString : '); if assigned(Args[i].VPointer) then Writeln(JLString(JLObject(Args[i].VPointer).toString)) else writeln('nil') end;
      vtPChar      : begin Write ('PCHar, value : '); Writeln(unicodestring(Ansistring(Args[i].VPChar))); end;
      vtObject     : begin Write ('Object, toString : '); if assigned(Args[i].VObject) then Writeln(JLString(Args[i].VObject.toString)) else writeln('nil') end;
      vtClass      : begin Write ('Class reference, toString : '); Writeln(JLString(JLClass(Args[i].VClass).toString)); end;
      vtAnsiString : begin Write ('AnsiString, value :'); Writeln(unicodestring(AnsiString(Args[I].VAnsiString))); end;

{
      vtWideChar   : (VWideChar: WideChar);
      vtPWideChar  : (VPWideChar: PWideChar);
      vtCurrency   : (VCurrency: PCurrency);
      vtVariant    : (VVariant: PVariant);
      vtInterface  : (VInterface: Pointer);
      vtWideString : (VWideString: Pointer);
}
      vtInt64      : begin Write ('Int64, value : '); Writeln(args[i].VInt64^); end;
      vtQWord      : begin Write ('QWord, value : '); Writeln(int64(args[i].VQWord^)); end;
    else
      begin Write ('(Unknown) : '); Writeln(args[i].vtype); end;
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
  Write ('Size of VarRec : '); Writeln(Sizeof(TVarRec));
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
