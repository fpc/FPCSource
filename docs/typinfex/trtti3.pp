Program trtti1;

{$Mode Delphi}
{$M+}

Uses
  Rttiobj,Typinfo;

Procedure TestGet (O : TMyTestObject);

begin
//  PI:=O.ClassInfo;
  With O do
    begin
    Writeln ('Field properties :');
    Write ('Property booleanField    : ',BooleanField,' : ');
    Writeln(Ord(booleanField)=GetOrdProp(O,'booleanfield'));
    Write ('Property ByteField       : ',ByteField,' : ');
    Writeln(ByteField=GetOrdProp(O,'bytefield'));
    Write ('Property CharField       : ',CharField,' : ');
    Writeln(Ord(CharField)=GetOrdProp(O,'charfield'));
    Write ('Property WordField       : ',WordField,' : ');
    Writeln(WordField=GetOrdProp(O,'wordfield'));
    Write ('Property IntegerField    : ',IntegerField,' : ');
    Writeln(IntegerField=GetOrdProp(O,'integerfield'));
    Write ('Property LongintField    : ',LongintField,' : ');
    Writeln(LongIntField=GetOrdProp(O,'longintfield'));
    Write ('Property CardinalField   : ',CardinalField,' : ');
    Writeln(CardinalField=GetOrdProp(O,'Cardinalfield'));
    Write ('Property RealField       : ',RealField,' : ');
    Writeln(RealField=GetFloatProp(O,'RealField'));
    Write ('Property ExtendedField   : ',ExtendedFIeld,' : ');
    Writeln(ExtendedField=GetFloatProp(O,'ExtendedField'));
    Write ('Property AnsiStringField : ',AnsiStringField,' : ');
    Writeln(AnsiStringField=GetStrProp(O,'AnsiStringField'));
    Write ('Property MyEnumField     : ',ord(MyEnumField),' : ');
    Writeln (GetEnumProp(O,'myEnumField'));
    Writeln ('Method properties :');
    Write ('Property booleanMethod    : ',BooleanMethod,' : ');
    Writeln(Ord(booleanMethod)=GetOrdProp(O,'booleanMethod'));
    Write ('Property ByteMethod       : ',ByteMethod,' : ');
    Writeln(ByteMethod=GetOrdProp(O,'byteMethod'));
    Write ('Property CharMethod       : ',CharMethod,' : ');
    Writeln(Ord(CharMethod)=GetOrdProp(O,'charMethod'));
    Write ('Property WordMethod       : ',WordMethod,' : ');
    Writeln(WordMethod=GetOrdProp(O,'wordMethod'));
    Write ('Property IntegerMethod    : ',IntegerMethod,' : ');
    Writeln(IntegerMethod=GetOrdProp(O,'integerMethod'));
    Write ('Property LongintMethod    : ',LongintMethod,' : ');
    Writeln(LongIntMethod=GetOrdProp(O,'longintMethod'));
    Write ('Property CardinalMethod   : ',CardinalMethod,' : ');
    Writeln(CardinalMethod=GetOrdProp(O,'CardinalMethod'));
    Write ('Property RealMethod       : ',RealMethod,' : ');
    Writeln(RealMethod=GetFloatProp(O,'RealMethod'));
    Write ('Property ExtendedMethod   : ',ExtendedMethod,' : ');
    Writeln(ExtendedMethod=GetFloatProp(O,'ExtendedMethod'));
    Write ('Property AnsiStringMethod : ',AnsiStringMethod,' : ');
    Writeln(AnsiStringMethod=GetStrProp(O,'AnsiStringMethod'));
    Write ('Property MyEnumMethod     : ',ord(MyEnumMethod),' : ');
    Writeln (GetEnumProp(O,'myEnumMethod'));
    Writeln ('VirtualMethod properties :');
    Write ('Property booleanVirtualMethod    : ',BooleanVirtualMethod,' : ');
    Writeln(Ord(booleanVirtualMethod)=GetOrdProp(O,'booleanVirtualMethod'));
    Write ('Property ByteVirtualMethod       : ',ByteVirtualMethod,' : ');
    Writeln(ByteVirtualMethod=GetOrdProp(O,'byteVirtualMethod'));
    Write ('Property CharVirtualMethod       : ',CharVirtualMethod,' : ');
    Writeln(Ord(CharVirtualMethod)=GetOrdProp(O,'charVirtualMethod'));
    Write ('Property WordVirtualMethod       : ',WordVirtualMethod,' : ');
    Writeln(WordVirtualMethod=GetOrdProp(O,'wordVirtualMethod'));
    Write ('Property IntegerVirtualMethod    : ',IntegerVirtualMethod,' : ');
    Writeln(IntegerVirtualMethod=GetOrdProp(O,'integerVirtualMethod'));
    Write ('Property LongintVirtualMethod    : ',LongintVirtualMethod,' : ');
    Writeln(LongIntVirtualMethod=GetOrdProp(O,'longintVirtualMethod'));
    Write ('Property CardinalVirtualMethod   : ',CardinalVirtualMethod,' : ');
    Writeln(CardinalVirtualMethod=GetOrdProp(O,'CardinalVirtualMethod'));
    Write ('Property RealVirtualMethod       : ',RealVirtualMethod,' : ');
    Writeln(RealVirtualMethod=GetFloatProp(O,'RealVirtualMethod'));
    Write ('Property ExtendedVirtualMethod   : ',ExtendedVirtualMethod,' : ');
    Writeln(ExtendedVirtualMethod=GetFloatProp(O,'ExtendedVirtualMethod'));
    Write ('Property AnsiStringVirtualMethod : ',AnsiStringVirtualMethod,' : ');
    Writeln(AnsiStringVirtualMethod=GetStrProp(O,'AnsiStringVirtualMethod'));
    Write ('Property MyEnumVirtualMethod     : ',ord(MyEnumVirtualMethod),' : ');
    Writeln (GetEnumProp(O,'myEnumVirtualMethod'));
    end;
  end;

Var O : TMyTestObject;

begin
  O:=TMyTestObject.Create;
  testget(o);
end.
