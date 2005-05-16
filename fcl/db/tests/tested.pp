{
    $Id: tested.pp,v 1.3 2005/02/14 17:13:12 peter Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of the
    Free Pascal development team

    Tests the TDDGDataset component.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program testds;

uses db,ddg_ds,sysutils;

Procedure Log(Const Msg : String);
begin
  Writeln(Msg);
end;

Procedure DumpFieldDef(F : TfieldDef);

begin
  With F do
    begin
    Writeln ('Name              : ',Name);
    Writeln ('FieldNo           : ',FieldNo);
    Writeln ('Size              : ',Size);
    Writeln ('FieldClass        : ',FieldClass.ClassName);
    Writeln ('Required          : ',required);
    Writeln ('Precision         : ',Precision);
    Writeln ('DataType          : ',FieldTypeNames[DataType]);
    Writeln ('InternalCalcField : ',Internalcalcfield);
    end;
end;

Procedure DumpField(F : Tfield);

begin
  With F do
    begin
    writeln ('-------------------------------------');
    Writeln ('FieldName : ',FieldName);
    Writeln ('FieldNo   : ',FieldNo);
    Writeln ('Index     : ',Index);
    Writeln ('DataSize  : ',DataSize);
    Writeln ('Size      : ',Size);
    Writeln ('DataType  : ',FieldTypeNames[DataType]);
    Writeln ('Class     : ',ClassName);
    Writeln ('Required  : ',required);
    Writeln ('ReadOnly  : ',ReadOnly);
    Writeln ('Visible   : ',Visible);
    end;
end;

Procedure DumpFieldData (F : TField);

begin
  With F Do
    begin
    Writeln ('Field     : ',FieldName);
    Writeln ('Data type : ',FieldTypeNames[DataType]);
    Writeln ('As String : ',Asstring);
    Case Datatype of
      ftSmallint, ftInteger, ftWord : Writeln ('As longint : ',AsLongint);
      ftBoolean : Writeln ('As Boolean : ',AsBoolean);
      ftFloat : Writeln ('As Float : ',AsFloat);
      ftDate, ftTime, ftDateTime : Writeln ('As DateTime : ',DateTimeToStr(AsDateTime));
    end;
    end;
end;

procedure DumpFields (DS : TDataset);

Var I : longint;

begin
  With DS do
    begin
    Writeln('Dumping fields');
    For I:=0 to FieldCount-1 do
      DumpFieldData(Fields[i]);
    end;
end;

Var
  Data : TDDGdataset;
  I,Count : longint;
  Bookie : TBookMarkStr;

Procedure ScrollForward;

begin
  Writeln ('Browsing Forward:');
  Writeln ('------------------');
  With Data do
    While NOT EOF do
      begin
      Writeln ('================================================');
      For I:=0 to FieldCount-1 do
        DumpFieldData(Fields[I]);
      Next;
      end;
end;

Procedure ScrollBackWard;

begin
  Writeln ('Browsing Backward:');
  Writeln ('-------------------');
  With Data do
    While NOT BOF do
      begin
      For I:=0 to FieldCount-1 do
        DumpFieldData(Fields[I]);
      Prior;
      end;
end;

begin
  if paramcount<>1 then
    begin
    Writeln ('Usage : testds tablename');
    Halt(1);
    end;
  Log ('Creating Dataset');
  Data:=TDDGDataset.Create(Nil);
  With Data do
    begin
    Log('Setting Tablename');
    TableName:=Paramstr(1);
    Log('Opening Dataset');
    Open;
    Log('Dumping fielddefs : ');
    Writeln ('Fielddefs count : ',FieldDefs.Count);
    For I:=0 to FieldDefs.Count-1 do
      DumpFieldDef(FieldDefs.Items[i]);
    Writeln ('Fields count : ',FieldCount);
    For I:=0 to FieldCount-1 do
      DumpField(Fields[i]);
    ScrollForward;
    ScrollBackWard;
    Writeln ('Doing append');
    writeln ('------------');
    Append;
    FieldByName('Name').AsString:='AppendName';
    FieldByName('Height').AsFloat:=9.99E9;
    FieldByName('LongField').AsLongInt:=999;
    FieldByName('ShoeSize').AsLongInt:=999;
    FieldByName('WordField').AsLongInt:=999;
    FieldByName('BooleanField').AsBoolean:=False;
    FieldByName('DateTimeField').AsDateTime:=Now;
    FieldByName('DateField').AsDateTime:=Date;
    FieldByName('TimeField').AsDateTime:=Time;
    Writeln ('End of append, going to post');
    Post;
    DumpFields(Data);
    Writeln ('Doing Last');
    Writeln ('----------');
    Last;
    DumpFields(Data);
    Writeln ('Doing Prior');
    Writeln ('----------');
    Prior;
    DumpFields(Data);
    Writeln ('Doing Insert at position 8');
    writeln ('--------------------------');
    first;
    for I:=1 to 7 do
      Next;
    Insert;
    FieldByName('Name').AsString:='Insertname';
    FieldByName('Height').AsFloat:=8.99E8;
    FieldByName('LongField').AsLongInt:=888;
    FieldByName('ShoeSize').AsLongInt:=888;
    FieldByName('WordField').AsLongInt:=888;
    FieldByName('BooleanField').AsBoolean:=True;
    FieldByName('DateTimeField').AsDateTime:=Now;
    FieldByName('DateField').AsDateTime:=Date;
    FieldByName('TimeField').AsDateTime:=Time;
    Post;
    Writeln ('Doing field dump');
    writeln ('----------------');
    DumpFields(Data);
    Writeln ('Doing Prior');
    Writeln ('-----------');
    Prior;
    DumpFields(Data);
    Writeln ('Doing Next');
    Writeln ('----------');
    Next;
    DumpFields(Data);
    Writeln ('Doing Next');
    Writeln ('----------');
    Next;
    DumpFields(Data);
    Writeln ('Doing Edit at position 5');
    writeln ('-------------------------');
    first;
    for I:=1 to 4 do
      Next;
    Edit;
    FieldByName('Name').AsString:='Editname';
    FieldByName('Height').AsFloat:=3.33E3;
    FieldByName('LongField').AsLongInt:=333;
    FieldByName('ShoeSize').AsLongInt:=333;
    FieldByName('WordField').AsLongInt:=333;
    FieldByName('BooleanField').AsBoolean:=False;
    FieldByName('DateTimeField').AsDateTime:=Now;
    FieldByName('DateField').AsDateTime:=Date;
    FieldByName('TimeField').AsDateTime:=Time;
    Post;
    Writeln ('Doing field dump');
    writeln ('----------------');
    DumpFields(Data);
    Writeln ('Doing Prior');
    Writeln ('-----------');
    Prior;
    DumpFields(Data);
    Writeln ('Doing Next');
    Writeln ('----------');
    Next;
    DumpFields(Data);
    Writeln ('Doing Next');
    Writeln ('----------');
    Next;
    DumpFields(Data);
    Writeln ('Closing.');
    Close;
    end;
end.
{
  $Log: tested.pp,v $
  Revision 1.3  2005/02/14 17:13:12  peter
    * truncate log

}
