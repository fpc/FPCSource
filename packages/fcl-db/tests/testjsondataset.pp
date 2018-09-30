program testjsondataset;

{$DEFINE TESTCALCFIELDS}

uses sysutils, db, jsonparser, fpjson,fpjsondataset, extjsdataset;

Type

  { TApp }

  TApp = Class(TObject)
    Procedure DumpRecord(DS : TDataset);
    Procedure DumpRecords(DS : TDataset);
    Procedure Run;
  private
    procedure DoCalcFields(DataSet: TDataSet);
  end;

Procedure TApp.DumpRecord(DS : TDataset);

//Var
//  F : TField;

begin
//  For F in  DS.Fields do
//    Write(F.Name,' : ',F.IsNull,' ');
//  WriteLn;
  Writeln(
  {$IFDEF TESTCALCFIELDS}
          'Full name: ',DS.FieldByName('fullname').AsString,
  {$ENDIF}
          'First name: ',DS.FieldByName('firstname').AsString,
          ', Last name: ', DS.FieldByName('lastname').AsString,
          ', Children: ', DS.FieldByName('children').AsInteger,
          ', Birthday: ', DS.FieldByName('birthday').AsString
  );
end;

Procedure TApp.DumpRecords(DS : TDataset);

begin
  While not DS.EOF do
    begin
    Write(DS.RecNo,': ');
    DumpRecord(DS);
    DS.Next;
    end;
end;


Procedure TApp.Run;

Var
  DS : TExtJSJSONObjectDataSet;
  B : TBookmark;
  t: TDataLink;
  DSS : TDatasource;
{$IFDEF TESTCALCFIELDS}
  F : TField;
{$ENDIF}

begin

  DS:=TExtJSJSONObjectDataSet.Create(Nil);
  DS.MetaData:=GetJSON(' { "fields" : [ {"name": "firstname", "maxLen" : 100}, {"name": "lastname","maxLen" : 100}, '+
                       ' { "name" : "children", "type": "int" }, '+
                       ' { "name" : "birthday", "type": "date", "dateFormat": "yyyy\"-\"mm\"-\"dd" } ]}') as TJSONObject;
  DS.Rows:=GetJSON('[{"firstname" : "Michael", "lastname" : "Van Canneyt", "children" : 2, "birthday": "1970-07-07" },'+
                                  '  {"firstname" : "Mattias", "lastname" : "Gaertner", "children" : 0, "birthday" : "1970-07-08" }, '+
                                  '  {"firstname" : "Bruno", "lastname" : "Fierens", "children" : 3, "birthday" : "1970-07-09" },'+
                                  '  {"firstname" : "Detlef", "lastname" : "Overbeek", "children" : 2, "birthday" : "1950-07-08" }'+
                                  ' ]') as TJSONarray;
{$IFDEF TESTCALCFIELDS}
  F:=TStringField.Create(DS);
  F.FieldKind:=fkCalculated;
  F.Size:=200;
  F.FieldName:='fullname';
  F.Dataset:=DS;
  F:=TStringField.Create(DS);
  F.FieldKind:=fkData;
  F.Size:=200;
  F.FieldName:='firstname';
  F.Dataset:=DS;
  F:=TStringField.Create(DS);
  F.FieldKind:=fkData;
  F.Size:=200;
  F.FieldName:='lastname';
  F.Dataset:=DS;
  F:=TIntegerField.Create(DS);
  F.FieldKind:=fkData;
  F.FieldName:='children';
  F.Dataset:=DS;
  F:=TJSONDateField.Create(DS);
  TJSONDateField(F).DateFormat:='yyyy"-"mm"-"dd';
  F.FieldKind:=fkData;
  F.FieldName:='birthday';

  F.Dataset:=DS;
  DS.OnCalcFields:=@DoCalcFields;
{$ENDIF}
  DS.Open;
  Writeln('All records');
  DumpRecords(DS);
  Writeln('First record (expect Michael.)');
  DS.First;
  DumpRecord(DS);
  Writeln('Jump to last (expect detlef)');
  DS.Last;
  DumpRecord(DS);
  Writeln('Reverse order:');
  While not DS.BOF do
    begin
    DumpRecord(DS);
    DS.Prior;
    end;
  DS.Append;
  Writeln('Dumping record after APPEND (expect empty)');
  Writeln('Modified before dump (expect False): ',DS.Modified);
  DumpRecord(DS);
  DS.FieldByName('firstname').AsString:='Florian';
  Write('Old value of field first name (expect null): ');
  if DS.FieldByName('firstname').OldValue=Null then
    Writeln('Null')
  else
    Writeln(DS.FieldByName('firstname').OldValue);
  DS.FieldByName('lastname').AsString:='Klaempfl';
  DS.FieldByName('children').AsInteger:=1;
  DS.FieldByName('birthday').AsDateTime:=EncodeDate(1980,5,4);
  Writeln('Modified after (expect true): ',DS.Modified);
  Writeln('Dumping record before POST (Expect Florian)');
  DumpRecord(DS);
  DS.Post;
  Writeln('Dumping record after POST (Expect Florian)');
  DumpRecord(DS);
  Writeln('Jump to first (expect Michael)');
  DS.First;
  DumpRecord(DS);
  DS.Edit;
  Writeln('Dumping record after EDIT');
  Writeln('Modified before  (expect False): ',DS.Modified);
  DumpRecord(DS);
  DS.FieldByName('firstname').AsString:='Dolores';
  DS.FieldByName('lastname').AsString:='Nabokov';
  DS.FieldByName('children').AsInteger:=0;
  DS.FieldByName('birthday').AsDateTime:=EncodeDate(1943,2,14);
  Writeln('Modified after (expect true): ',DS.Modified);
  Writeln('Dumping record before POST (expect Dolores)');
  DumpRecord(DS);
  DS.Post;
  Writeln('Dumping record after POST (expect Dolores)');
  DumpRecord(DS);
  DS.Edit;
  Writeln('Dumping record after second EDIT (Expect Dolores)');
  DumpRecord(DS);
  Writeln('Modified before  (expect False): ',DS.Modified);
  DS.FieldByName('firstname').AsString:='Felicity';
  Writeln('Old value of field first name (expect Dolores): ', DS.FieldByName('firstname').OldValue);
  DS.FieldByName('lastname').AsString:='Brown';
  DS.FieldByName('children').AsInteger:=0;
  DS.FieldByName('birthday').AsDateTime:=EncodeDate(1943,2,14);
  Writeln('Modified after (expect true): ',DS.Modified);
  Writeln('Dumping record before Cancel (expect Filicity brown)');
  DumpRecord(DS);
  DS.Cancel;
  Writeln('Dumping record after Cancel (expect Dolores)');
  DumpRecord(DS);
  Writeln('Jump to first and dumping all records (expect Dolores first)');
  DS.First;
  DumpRecords(DS);
  Writeln('Jump to first  (expect Dolores)');
  DS.First;
  DumpRecord(DS);
  DS.Next;
  DS.Next;
  DS.Next;
  Writeln('Getting Bookmark (expect Detlef)');
  DumpRecord(DS);
  B:=DS.BookMark;
  DS.First;
  Writeln('Delete (expect Mattias)');
  DS.Delete;
  DumpRecord(DS);
  Writeln('Setting Bookmark (expect Detlef)');
  Writeln('BM value: ',PNativeInt(B)^);
  DS.BookMark:=B;
  DumpRecord(DS);
  Writeln('Jump to second (expect Bruno)');
  DS.First;
  DS.Next;
  DumpRecord(DS);
  DS.Insert;
  Writeln('Dumping record after second Insert (Expect empty)');
  Writeln('Modified (expect False): ',DS.Modified);
  DumpRecord(DS);
  DS.FieldByName('firstname').AsString:='Felicity';
  DS.FieldByName('lastname').AsString:='Brown';
  DS.FieldByName('children').AsInteger:=0;
  DS.FieldByName('birthday').AsDateTime:=EncodeDate(1963,4,6);
  Writeln('Modified (expect true): ',DS.Modified);
  Writeln('Dumping record before POST (expect Filicity)');
  DumpRecord(DS);
  DS.Post;
  Writeln('Dumping record after POST (expect Felicity)');
  DumpRecord(DS);
  Writeln('Jump to first and dumping all records (expect Mattias first, then Felicity)');
  DS.First;
  DumpRecords(DS);
  Writeln('Jump to first before edit');
  DS.First;
  DSS:=TDatasource.Create(Nil);
  DSS.DataSet:=DS;
  t:=TDataLink.Create;
  try
    Writeln('Buffercount');
    t.BufferCount := 10;
    t.DataSource := DSS;
    Writeln('Doing edit');
    t.Edit;
    Writeln('Modified (expect false): ',DS.Modified);
    Writeln('Done edit');
    t.ActiveRecord := 0;
    Writeln('Edit, expect Mattias');
    DumpRecord(DS);
    Writeln('Activerecord 1: expect Felicity');
    t.ActiveRecord := 1;
    DumpRecord(DS);
    Writeln('Activerecord 2: expect Bruno');
    t.ActiveRecord := 2;
    DumpRecord(DS);
    t.ActiveRecord := 0;
  Finally
    t.Free;
  end;
  t:=TDataLink.Create;
  try
    t.DataSource := DSS;
    DS.Last;
    Writeln('Last record :',DS.RecNo);
    Writeln('Activerecord :',T.ActiveRecord);
    DumpRecord(DS);
    DS.First;
    t.BufferCount := 3;
    DS.Last;
    Writeln('Last record after buffercount lessened:',DS.RecNo);
    Writeln('Activerecord :',T.ActiveRecord);
    DumpRecord(DS);
    t.ActiveRecord := 0;
  Finally
    t.Free;
  end;

end;

procedure TApp.DoCalcFields(DataSet: TDataSet);
begin
  Writeln('In calcfields callback');
  Dataset.FieldByName('FullName').AsString:= Dataset.FieldByName('firstName').AsString+' '+Dataset.FieldByName('lastname').AsString;
end;

begin
  With Tapp.Create do
    Run;
end.

