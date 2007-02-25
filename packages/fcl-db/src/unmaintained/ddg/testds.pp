{
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
    Writeln ('Going to last :');
    writeln ('---------------');
    Last;
    ScrollBackWard;
    ScrollForward;
    Writeln ('Going to first:');
    First;
    Count:=0;
    Writeln ('Browsing Forward:');
    Writeln ('------------------');
    With Data do
      While NOT EOF do
        begin
        Inc(Count);
        If Count=50 then
          begin
          Writeln ('Setting bookmark on record');
          Bookie:=Bookmark;
          Writeln ('Got data : "',Bookie,'"');
          end;
        For I:=0 to FieldCount-1 do
          DumpFieldData(Fields[I]);
        Next;
        end;
    Writeln ('Jumping to bookmark',Bookie);
    BookMark:=Bookie;
    Writeln ('Dumping Record : ');
      For I:=0 to FieldCount-1 do
        DumpFieldData(Fields[I]);
    Next;
    Writeln ('Dumping Next Record : ');
    For I:=0 to FieldCount-1 do
      DumpFieldData(Fields[I]);
    Prior;
    Prior;
    Writeln ('Dumping Previous Record : ');
    For I:=0 to FieldCount-1 do
      DumpFieldData(Fields[I]);
    Log('Closing Dataset');
    Close;
    Log('End.');
    Free;
    end;
end.
