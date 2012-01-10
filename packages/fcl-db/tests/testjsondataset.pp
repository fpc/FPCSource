program testjsondataset;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, DB, fpjsondataset, fpjson, jsonparser;

Function ExtractData(Const AFileName : string) : TJSONObject;

Var
  F : TFIleStream;
  P : TJSONParser;
  D : TJSONData;

begin
  Result:=Nil;
  F:=TFileStream.Create(AFileName,fmOpenRead);
  try
    P:=TJSONParser.Create(F);
    try
      D:=P.Parse;
      if (D.JSONType=jtObject) then
        Result:=D as TJSONObject
      else
        FreeAndNil(D);
    finally
      P.Free;
    end;
  finally
    F.Free;
  end;
end;
Procedure DumpDataset(DS : TDataset);

Var
  I,J : Integer;

begin
  I:=0;
  Writeln('Dataset contains ',DS.RecordCount,' records');
  While not DS.EOF do
    begin
    Inc(I);
    Writeln('=== Record ',I,' : ',DS.RecNo,' ===');
    For J:=0 to DS.Fields.Count-1 do
      With DS.Fields[J] do
        Writeln(FieldName,' : ',AsString);
    DS.Next;
    end;
  Writeln('Dataset contained ',I,' records');
end;

Procedure DoTest4(Const AFileName : string);

Var
  DS : TExtjsJSONObjectDataset;

begin
  DS:=TExtjsJSONObjectDataset.Create(Nil);
  try
    DS.LoadFromFile(AFileName);
    DS.Open;
    DumpDataset(DS);
  finally
    DS.Free;
  end;
end;

Procedure DoTest1(Const AFileName : string);

Var
  D,M : TJSONObject;
  DS : TExtjsJSONObjectDataset;
  I,J : Integer;
  F : TFieldDef;

begin
  D:=ExtractData(AFileName);
  try
    DS:=TExtjsJSONObjectDataset.Create(Nil);
    try
      DS.Rows:=D.Arrays['rows'];
      DS.Metadata:=D.Objects['metaData'];
      DS.OwnsData:=False;
      DS.Open;
      For I:=0 to DS.FieldDefs.Count-1 do
        begin
        F:=DS.FieldDefs[i];
        Writeln('FieldDefs.Add(''',F.Name,''',',F.DataType,',',F.Size,');');
        end;
      DumpDataset(DS);
    finally
      DS.Free;
    end;
  finally
    D.Free;
  end;
end;

Procedure DoTest2(Const AFileName : string);

Var
  D,M : TJSONObject;
  DS : TExtjsJSONObjectDataset;
  I,J : Integer;
  F : TFieldDef;
begin
  D:=ExtractData(AFileName);
  DS:=TExtjsJSONObjectDataset.Create(Nil);
  DS.Rows:=D.Arrays['rows'];
  With DS do
    begin
    FieldDefs.Add('ID',ftLargeint,0);
    FieldDefs.Add('Name',ftString,20);
    FieldDefs.Add('Email',ftString,30);
    end;
  DS.Open;
  DumpDataset(DS);
end;

Procedure DoTest3(Const AFileName : string);

Var
  DS : TExtjsJSONObjectDataset;
  I,J : Integer;
  F : TFieldDef;

begin
  DS:=TExtjsJSONObjectDataset.Create(Nil);
  try
    With DS do
      begin
      FieldDefs.Add('ID',ftLargeint,0);
      FieldDefs.Add('Name',ftString,20);
      FieldDefs.Add('Email',ftString,30);
      Open;
      // Record 1
      Append;
      FieldByName('ID').AsInteger:=3;
      FieldByName('Name').AsString:='Michael';
      FieldByName('Email').AsString:='michael@freepascal.org';
      Post;
      // Record 2
      Append;
      FieldByName('ID').AsInteger:=4;
      FieldByName('Name').AsString:='jonas';
      FieldByName('Email').AsString:='jonas@freepascal.org';
      Post;
      DumpDataset(DS);
      First;
      // insert record 1
      Insert;
      FieldByName('ID').AsInteger:=1;
      FieldByName('Name').AsString:='Florian';
      FieldByName('Email').AsString:='Florian@freepascal.org';
      Post;
      DumpDataset(DS);
      Writeln('First');
      First;
      Writeln('Editing record ', RecNo,' ',FieldByName('Name').AsString);
      Edit;
      FieldByName('ID').AsInteger:=12;
      FieldByName('Name').AsString:='Marco';
      FieldByName('Email').AsString:='Marco@stack.nl';
      Post;
      First;
      DumpDataset(DS);
      First;
      Next;
      Writeln('Deleting record ', RecNo,' ',FieldByName('Name').AsString);
      Delete;
      First;
      DumpDataset(DS);
      SaveToFile(AFileName,True);
      end;
  finally
    DS.Free
  end;
end;
begin
  Writeln('Test 1');
  DoTest1('test.json');
  Writeln('Test 2');
  DoTest2('test.json');
  Writeln('Test 3');
  DoTest3('test3.json');
  Writeln('Test 4');
  DoTest4('test.json');
end.

