{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                             TStrings                                     *}
{****************************************************************************}


function TStrings.GetCommaText: string;

begin
end;



function TStrings.GetName(Index: Integer): string;

begin
end;



Function TStrings.GetValue(const Name: string): string;

begin
end;



Procedure TStrings.ReadData(Reader: TReader);

begin
end;



Procedure TStrings.SetCommaText(const Value: string);

begin
end;



Procedure TStrings.SetStringsAdapter(const Value: IStringsAdapter);

begin
end;



Procedure TStrings.SetValue(const Name, Value: string);

begin
end;



Procedure TStrings.WriteData(Writer: TWriter);

begin
end;



Procedure TStrings.DefineProperties(Filer: TFiler); 

begin
end;



Procedure TStrings.Error(const Msg: string; Data: Integer);

begin
end;



Function TStrings.GetCapacity: Integer; 

begin
end;



Function TStrings.GetObject(Index: Integer): TObject; 

begin
end;



Function TStrings.GetTextStr: string; 

begin
end;



Procedure TStrings.Put(Index: Integer; const S: string); 

begin
end;



Procedure TStrings.PutObject(Index: Integer; AObject: TObject); 

begin
end;



Procedure TStrings.SetCapacity(NewCapacity: Integer); 

begin
end;



Procedure TStrings.SetTextStr(const Value: string); 

begin
end;



Procedure TStrings.SetUpdateState(Updating: Boolean); 

begin
end;



destructor TSTrings.Destroy; 

begin
  inherited destroy;
end;



Function TStrings.Add(const S: string): Integer; 

begin
  Result:=Count;
  Insert (Count,S);
end;



Function TStrings.AddObject(const S: string; AObject: TObject): Integer; 

begin
  Result:=Add(S);
  Objects[result]:=AObject;
end;



Procedure TStrings.Append(const S: string);

begin
  Add (S);
end;



Procedure TStrings.AddStrings(TheStrings: TStrings); 

Var Runner : longint;

begin
  For Runner:=0 to TheStrings.Count-1 do
    self.AddObject (Thestrings[Runner],TheStrings.Objects[Runner]);
end;



Procedure TStrings.Assign(Source: TPersistent); 

begin
  If Source is TStrings then
    begin
    clear;
    AddStrings(TStrings(Source));
    exit;
    end;
  Inherited Assign(Source);
end;



Procedure TStrings.BeginUpdate;

begin
end;



Procedure TStrings.EndUpdate;

begin
end;



Function TStrings.Equals(TheStrings: TStrings): Boolean;

Var Runner,Nr : Longint;

begin
  Result:=False;
  Nr:=Self.Count;
  if Nr<>TheStrings.Count then exit;
  For Runner:=0 to Nr-1 do 
    If Strings[Runner]<>TheStrings[Runner] then exit;
  Result:=True;
end;



Procedure TStrings.Exchange(Index1, Index2: Integer); 

Var
  Obj : TObject;
  Str : String;

begin
  Obj:=Objects[Index1];
  Str:=Strings[Index1];
  Objects[Index1]:=Objects[Index2];
  Strings[Index1]:=Strings[Index2];
  Objects[Index2]:=Obj;
  Strings[Index2]:=Str;
end;



Function TStrings.GetText: PChar; 

begin
//!!  Result:=StrNew(Pchar(Self.Text));
end;



Function TStrings.IndexOf(const S: string): Integer; 


begin
  Result:=0;
  While (Result<Count) and (Strings[Result]<>S) do Result:=Result+1;
  if Result=Count then Result:=-1;
end;



Function TStrings.IndexOfName(const Name: string): Integer;

Var len : longint;

begin
  Result:=0;
  while (Result<Count) do
    begin
    len:=pos('=',Strings[Result])-1;
    if (len>0) and (Name=Copy(Strings[Result],1,Len)) then exit;
    inc(result);
    end;
  result:=-1; 
end;



Function TStrings.IndexOfObject(AObject: TObject): Integer;

begin
  Result:=0;
  While (Result<count) and (Objects[Result]<>AObject) do Result:=Result+1;
  If Result=Count then Result:=-1;
end;



Procedure TStrings.InsertObject(Index: Integer; const S: string;
  AObject: TObject);

begin
  Insert (Index,S);
  Objects[Index]:=AObject;
end;



Procedure TStrings.LoadFromFile(const FileName: string); 

Var TheStream : TFileStream;

begin
  TheStream:=TFileStream.Create(FileName,fmOpenRead);
  LoadFromStream(TheStream);
  TheStream.Free;
end;



Procedure TStrings.LoadFromStream(Stream: TStream); 

begin
  Text:=Stream.ReadAnsiString;
end;



Procedure TStrings.Move(CurIndex, NewIndex: Integer); 

Var Obj : TObject;
    Str : String;

begin
   Obj:=Objects[CurIndex];
   Str:=Strings[CurIndex];
   Delete(Curindex);
   InsertObject(NewIndex,Str,Obj);  
end;



Procedure TStrings.SaveToFile(const FileName: string); 

Var TheStream : TFileStream;

begin
  TheStream:=TFileStream.Create(FileName,fmCreate);
  SaveToStream(TheStream);
  TheStream.Free;
end;



Procedure TStrings.SaveToStream(Stream: TStream); 

begin
  Stream.WriteAnsiString(Text);
end;



Procedure TStrings.SetText(TheText: PChar); 

begin
end;


{****************************************************************************}
{*                             TStringList                                  *}
{****************************************************************************}



Procedure TStringList.ExchangeItems(Index1, Index2: Integer);

begin
end;



Procedure TStringList.Grow;

begin
end;



Procedure TStringList.QuickSort(L, R: Integer);

begin
end;



Procedure TStringList.InsertItem(Index: Integer; const S: string);

begin
end;



Procedure TStringList.SetSorted(Value: Boolean);

begin
end;



Procedure TStringList.Changed; 

begin
end;



Procedure TStringList.Changing; 

begin
end;



Function TStringList.Get(Index: Integer): string; 

begin
end;



Function TStringList.GetCapacity: Integer; 

begin
end;



Function TStringList.GetCount: Integer; 

begin
end;



Function TStringList.GetObject(Index: Integer): TObject; 

begin
end;



Procedure TStringList.Put(Index: Integer; const S: string); 

begin
end;



Procedure TStringList.PutObject(Index: Integer; AObject: TObject); 

begin
end;



Procedure TStringList.SetCapacity(NewCapacity: Integer); 

begin
end;



Procedure TStringList.SetUpdateState(Updating: Boolean); 

begin
end;



destructor TStringList.Destroy; 

begin
end;



Function TStringList.Add(const S: string): Integer; 

begin
end;



Procedure TStringList.Clear; 

begin
end;



Procedure TStringList.Delete(Index: Integer); 

begin
end;



Procedure TStringList.Exchange(Index1, Index2: Integer); 

begin
end;



Function TStringList.Find(const S: string; var Index: Integer): Boolean; 

begin
end;



Function TStringList.IndexOf(const S: string): Integer; 

begin
end;



Procedure TStringList.Insert(Index: Integer; const S: string); 

begin
end;



Procedure TStringList.Sort; 

begin
end;
{
  $Log$
  Revision 1.2  1998-05-06 12:58:53  michael
  + Initial implementation

  Revision 1.1  1998/05/04 14:30:12  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
