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
end;



Function TStrings.Add(const S: string): Integer; 

begin
end;



Function TStrings.AddObject(const S: string; AObject: TObject): Integer; 

begin
end;



Procedure TStrings.Append(const S: string);

begin
end;



Procedure TStrings.AddStrings(TheStrings: TStrings); 

begin
end;



Procedure TStrings.Assign(Source: TPersistent); 

begin
end;



Procedure TStrings.BeginUpdate;

begin
end;



Procedure TStrings.EndUpdate;

begin
end;



Function TStrings.Equals(TheStrings: TStrings): Boolean;

begin
end;



Procedure TStrings.Exchange(Index1, Index2: Integer); 

begin
end;



Function TStrings.GetText: PChar; 

begin
end;



Function TStrings.IndexOf(const S: string): Integer; 

begin
end;



Function TStrings.IndexOfName(const Name: string): Integer;

begin
end;



Function TStrings.IndexOfObject(AObject: TObject): Integer;

begin
end;



Procedure TStrings.InsertObject(Index: Integer; const S: string;
  AObject: TObject);

begin
end;



Procedure TStrings.LoadFromFile(const FileName: string); 

begin
end;



Procedure TStrings.LoadFromStream(Stream: TStream); 

begin
end;



Procedure TStrings.Move(CurIndex, NewIndex: Integer); 

begin
end;



Procedure TStrings.SaveToFile(const FileName: string); 

begin
end;



Procedure TStrings.SaveToStream(Stream: TStream); 

begin
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
  Revision 1.1  1998-05-04 14:30:12  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
