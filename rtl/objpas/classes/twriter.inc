{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

(*Procedure TTextWriter.WriteLn(Const Msg : String);

Const CRLF = #10;

begin
  Write(Msg+CRLF);
end;

Procedure TTextWriter.Write(Const Msg : String);

Var S : String;

begin
  S:=FPrefix+Msg;
  FStream.Write(Pointer(S)^,Length(S));
end;


Procedure TTextWriter.WriteFmt(Fmt : String; Args :  Array of const);

begin
  Writeln(Format(Fmt,Args));
end;

Procedure TTextWriter.StartObject(Const AClassName, AName : String);

begin
  WriteFmt('Object %s %s',[AName,AClassName]);
  FPrefix:=FPrefix+'  ';
end;

Procedure TTextWriter.EndObject;

Var L : longint;

begin
  L:=Length(FPrefix);
  If L>2 Then
    SetLength(FPrefix,L-2);
  Writeln('end');
end;

Procedure TTextWriter.StartCollection(Const AName : String);

begin
  WriteFmt('%s = (',[AName]);
  FPrefix:=FPrefix+'  ';
end;

Procedure TTextWriter.StartCollectionItem;

begin
end;

Procedure TTextWriter.EndCollectionItem;

begin
end;

Procedure TTextWriter.EndCollection;

Var L : longint;

begin
  L:=Length(FPrefix);
  If L>2 Then
    SetLength(FPrefix,L-2);
  Writeln(')');
end;


Procedure TTextWriter.WritePropName(const PropName: string);

begin
  Writeln(PropName);
end;

Constructor TTextWriter.Create(S : TStream);

begin
  Inherited Create;
  FStream:=S;
  FPrefix:='';
end;

Destructor TTextWriter.Destroy;

begin
end;

Procedure TTextWriter.WriteIntegerProperty(Const Name : Shortstring;Value : Longint);

begin
  WriteFmt('%s = %d',[Name,Value]);
end;

Procedure TTextWriter.WriteSetProperty (Const Name : ShortString;Value : longint; BaseType : TTypeInfo);

begin
  //!! needs implementing.
  WriteFmt('%s = []',[Name]);
end;

Procedure TTextWriter.WriteEnumerationProperty (Const Name : ShortString;Value : Longint; Const EnumName : ShortSTring);

begin
  WriteFmt('%s = %s',[Name,EnumName])
end;

Procedure TTextWriter.WriteStringProperty(Const Name : ShortString; Const Value : String);

Type
  TMode = (quoted,unquoted);

Var
  Mode : TMode;
  S : String;
  I,L : Longint;
  c : char;

   Procedure Add (A : String);

   begin
     S:=S+A;
   end;

begin
  L:=Length(Value);
  Mode:=unquoted;
  S:=Name+' = ';
  For I:=1 to L do
    begin
    C:=Value[i];
    If (ord(C)>31) and (Ord(c)<=128) and (c<>'''') then
      begin
      If mode=Quoted then
        Add(c)
      else
        begin
        Add(''''+c);
        mode:=quoted
        end
      end
    else
      begin
      If Mode=quoted then
        begin
        Add('''');
        mode:=unquoted;
        end;
      Add(Format('#%d',[ord(c)]));
      end;
    If Length(S)>72 then
      begin
      if mode=quoted then
        Add ('''');
      Add('+');
      Writeln(S);
      Mode:=unQuoted;
      end;
    end;
 if mode=quoted then Add('''');
 Writeln(S);
end;

Procedure TTextWriter.WriteFloatProperty(Const Name : ShortString; Value : Extended);

begin
  WriteFmt('%s = %e',[Name,Value])
end;

Procedure TTextWriter.WriteCollectionProperty(Const Name : ShortString;Value : TCollection);

begin

end;

Procedure TTextWriter.WriteClassProperty(Instance : TPersistent;Propinfo :PPropInfo);

begin
end;

Procedure TTextWriter.WriteComponentProperty(Const Name : ShortSTring; Value : TComponent);

begin
  WriteFmt ('%s = %s',[Name,Value.Name]);
end;

Procedure TTextWriter.WriteNilProperty(Const Name : Shortstring);

begin
  system.Writeln(stderr,'Nil : ',Name);
  WriteFmt ('%s = Nil',[Name])
end;

Procedure TTextWriter.WriteMethodProperty(Const Name,AMethodName : ShortString);

begin
  WriteFmt ('%s = %s',[Name,AMethodName]);
end;*)

