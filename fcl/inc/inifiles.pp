{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael A. Hess

    adapted from code by Stephan Schneider

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit IniFiles;

{$mode objfpc}
{$H+}

interface

uses Classes;

type

{ TIniFile class }

   TIniFile = class(TObject)
   private
     FEscapeLineFeeds : Boolean;
     FFileName   : string;
     FStream     : TStream;
     FFileBuffer : TStringList;
     function GetName(const line : string) : string;
     function GetValue(const line, name : string) : string;
     function IsComment(const line : string) : boolean;
     function IsSection(const line : string) : boolean;
     function GetSectionIndex(const section : string) : integer;
   protected
     procedure SetFileName(const fn:string);
     procedure SetStream(s:TStream);
     procedure LoadFromFile;
     procedure SaveToFile;
     procedure LoadFromStream;
     procedure SaveToStream;
   public
     constructor Create(const theFileName : string);
     constructor Create(s:TStream);
     destructor Destroy; override;
     procedure DeleteKey(const section, ident : string);
     procedure EraseSection(const section : string);
     function ReadBool(const section, ident : string; defaultValue : boolean) : boolean;
     function ReadInteger(const section, ident : string; defaultValue : longint) : longint;
     procedure ReadSection(const section : string; strings : TStrings);
     procedure ReadSections(strings : TStrings);
     procedure ReadSectionValues(const section : string; strings : TStrings);
     procedure ReadSectionRaw(const section : string; strings : TStrings);
     function ReadString(const section, ident, defaultValue : string) : string;
     procedure WriteBool(const section, ident : string; value : boolean);
     procedure WriteInteger(const section, ident : string; value : longint);
     procedure WriteString(const section, ident, value : string);
     property FileName : String read FFileName;
     property EscapeLineFeeds : Boolean Read FEscapeLineFeeds Write FEscapeLineFeeds default false;
   end;

implementation

uses SysUtils;

const
   brackets  : array[0..1] of Char = ('[', ']');
   separator : Char = '=';
   comment   : Char = ';';


{ TIniFile }

constructor TIniFile.Create(const theFileName : string);
begin
   FFileName := theFileName;
   FStream:=nil;
   FEscapeLineFeeds:=False;
   FFileBuffer := TStringList.Create;

   if FileExists(fileName) then
      LoadFromFile;
end;

constructor TIniFile.Create(s:TStream);
begin
   FFileName := '';
   FStream:=s;
   FEscapeLineFeeds:=False;
   FFileBuffer := TStringList.Create;
   LoadFromStream;
end;

destructor TIniFile.Destroy;
begin
   FFileBuffer.Free;
end;

function TIniFile.GetName(const line : string) : string;
var
   index,index2 : integer;
begin
   Result := '';
   index := Pos(separator, line);
   if index <> 0 then
    begin
      index2:=Pos(comment, line);
      if (index2=0) or (index2>index) then
       result := Trim(Copy(line, 1, index - 1));
    end;
end;

function TIniFile.GetValue(const line, name : string) : string;
var
   index1,index2,index3 : integer;
begin
   result := '';
   if (line <> '') and (name <> '') then
   begin
      index1 := Pos(name, line);
      index2 := Pos(separator, line);
      index3 := Pos(comment, line);
      if index3=0 then
       index3:=MaxInt;
      if (index1 <> 0) and (index2 <> 0) and (index2 > index1) then
         result := Trim(Copy(line, index2 + 1, index3));
   end;
end;

function TIniFile.IsSection(const line : string) : boolean;
var
   str : string;
begin
   result := False;
   if line <> '' then
   begin
      str := Trim(line);
      if (str[1] = brackets[0]) and (str[Length(str)] = brackets[1]) then
         result := True;
   end;
end;

function TIniFile.IsComment(const line : string) : boolean;
var
   str : string;
begin
   result := False;
   if line <> '' then
   begin
      str := Trim(line);
      result := (str[1]=comment);
   end;
end;

function TIniFile.GetSectionIndex(const section : string) : integer;
begin
   result := FFileBuffer.IndexOf(brackets[0] + section + brackets[1]);
end;

{ Load/Save }

procedure TIniFile.SetFileName(const fn:string);
begin
  FFileName:=fn;
end;

procedure TIniFile.SetStream(s:TStream);
begin
  FStream:=s;
end;

procedure TIniFile.LoadFromFile;
begin
  if FFileName<>'' then
   FFileBuffer.LoadFromFile(FFileName);
end;

procedure TIniFile.SaveToFile;
begin
  if FFileName<>'' then
   FFileBuffer.SaveToFile(FFileName);
end;

procedure TIniFile.LoadFromStream;
begin
  if assigned(FStream) then
   FFileBuffer.LoadFromStream(FStream);
end;

procedure TIniFile.SaveToStream;
begin
  if assigned(FStream) then
   FFileBuffer.SaveToStream(FStream);
end;

{ Read all Names of one Section }

procedure TIniFile.ReadSection(const section : string; strings : TStrings);
var
   index : integer;
   name : string;
begin
   strings.BeginUpdate;
   try
      strings.Clear;
      if FFileBuffer.Count > 0 then
      begin
         index := GetSectionIndex(section);
         if index <> -1 then
         begin
            Inc(index);
            while (index < FFileBuffer.Count) and not IsSection(FFileBuffer[index]) do
            begin
               name := GetName(FFileBuffer[index]);
               if name <> '' then
                  strings.Add(name);
               Inc(index);
            end;
         end;
      end;
   finally
      strings.EndUpdate;
   end;
end;

{ Read all Sections of the Ini-File }

procedure TIniFile.ReadSections(strings : TStrings);
var
   index : integer;
   section : string;
begin
   strings.BeginUpdate;
   try
      strings.Clear;
      if FFileBuffer.Count > 0 then
      begin
         index := 0;
         while (index < FFileBuffer.Count) do
         begin
            if IsSection(FFileBuffer[index]) then
            begin
               section := Trim(FFileBuffer[index]);
               Delete(section, 1, 1);
               Delete(section, Length(section), 1);
               strings.Add(Trim(section));
            end;
            Inc(index);
         end;
      end;
   finally
      strings.EndUpdate;
   end;
end;

{ Reads a String-Value of "ident" in one "section".
  The result is "defaultValue" if
  o section doesn't exists
  o ident doesn't exists
  o ident doesn't have any assigned value }

function TIniFile.ReadString(const section, ident, defaultValue : string) : string;
var
   index : integer;
   value : string;
begin
   result := defaultValue;
   if FFileBuffer.Count > 0 then
   begin
      index := GetSectionIndex(section);
      if index <> -1 then
      begin
         Inc(index);
         while (index < FFileBuffer.Count) and not IsSection(FFileBuffer[index]) do
         begin
            if CompareText(GetName(FFileBuffer[index]),ident)=0 then
            begin
              value := GetValue(FFileBuffer[index], ident);
              if value <> '' then
               begin
                 result := value;
                 if EscapeLineFeeds and (result[length(result)]='\') then
                  begin
                    inc(index);
                    while (index < FFileBuffer.Count) and (result[length(result)]='\') do
                     begin
                       result:=Copy(result,1,length(result)-1)+Trim(FFileBuffer[index]);
                       inc(index);
                     end;
                  end;
               end;
              break;
            end;
            Inc(index);
         end;
      end;
   end;
end;

{ Reads an Integer-Value of Ident in one Section }

function TIniFile.ReadInteger(const section, ident : string; defaultValue : longint) : longint;
var
   intStr : string;
begin
   intStr := ReadString(section, ident, '');
   { convert a Hex-Value }
   if (Length(intStr) > 2) and (intStr[1] = '0') and ((intStr[2] = 'X') or (intStr[2] = 'x')) then
      intStr := '$' + Copy(intStr, 3, Maxint);
   result := StrToIntDef(intStr, defaultValue);
end;

{ Reads a Bool-Value of Ident in one Section }

function TIniFile.ReadBool(const section, ident : string; defaultValue : boolean) : boolean;
begin
   result := ReadInteger(section, ident, Ord(defaultValue)) <> 0;
end;

{ Reads all Names + Values of one Section }

procedure TIniFile.ReadSectionValues(const section : string; strings : TStrings);
var
   name : string;
   value : string;
   index : integer;
begin
   strings.BeginUpdate;
   try
      strings.Clear;
      if FFileBuffer.Count > 0 then
      begin
         index := GetSectionIndex(section);
         if index <> -1 then
         begin
            Inc(index);
            while (index < FFileBuffer.Count) and not IsSection(FFileBuffer[index]) do
            begin
               name := GetName(FFileBuffer[index]);
               if name <> '' then
               begin
                  value := GetValue(FFileBuffer[index], name);
                  strings.Add(name + separator + value);
               end;
               Inc(index);
            end;
         end;
      end;
   finally
      strings.EndUpdate;
   end;
end;

procedure TIniFile.ReadSectionRaw(const section : string; strings : TStrings);
var
   eols,index : integer;
begin
   strings.BeginUpdate;
   try
      eols:=0;
      strings.Clear;
      if FFileBuffer.Count > 0 then
      begin
         index := GetSectionIndex(section);
         if index <> -1 then
         begin
            Inc(index);
            while (index < FFileBuffer.Count) and not IsSection(FFileBuffer[index]) do
             begin
               { Skip empty lines at the end of the section }
               if FFileBuffer[index]='' then
                inc(eols)
               else
                begin
                  while eols>0 do
                   begin
                     Strings.Add('');
                     dec(eols);
                   end;
                  if not IsComment(FFileBuffer[index]) then
                   strings.Add(FFileBuffer[index]);
                end;
               Inc(index);
             end;
         end;
      end;
   finally
      strings.EndUpdate;
   end;
end;

{ Writes a String-Value for Ident in one Section.
  Note: If Section and/or Ident don't exist, they will be placed in the Ini-File }

procedure TIniFile.WriteString(const section, ident, value : string);
var
   index : integer;
begin
   index := GetSectionIndex(section);
   { Section exists }
   if index <> -1 then
   begin
      Inc(index);
      while (index < FFileBuffer.Count) and not IsSection(FFileBuffer[index]) and
            (GetName(FFileBuffer[index]) <> ident) do
         Inc(index);
      if (index >= FFileBuffer.Count) or IsSection(FFileBuffer[index]) then
      begin         { End of File or ident doesn't exists in the section }
         if ident <> '' then
            FFileBuffer.Insert(index, ident + separator + value);
      end
      else if ident <> '' then   { Ident does exists in the section }
         FFileBuffer[index] := ident + separator + value;
   end
   else   { section doesn't exists, so add new [section] with ident=value }
   begin
      FFileBuffer.Add('');
      FFileBuffer.Add(brackets[0] + section + brackets[1]);
      if ident <> '' then
         FFileBuffer.Add(ident + separator + value);
   end;
   SaveToFile;
end;

{ Writes an Integer-Value for Ident in one Section }

procedure TIniFile.WriteInteger(const section, ident : string; value : longint);
begin
   WriteString(section, ident, IntToStr(value));
end;

{ Writes a Bool-Value for Ident in one Section }

procedure TIniFile.WriteBool(const section, ident : string; value : boolean);
const
   values: array[boolean] of string = ('0', '1');
begin
   WriteString(section, ident, values[Value]);
end;

{ Deletes the value of ident in one section.
  Note: Only if section and ident exist, the value of ident will be set to NULL }

procedure TIniFile.DeleteKey(const section, ident : string);
var
   index : integer;
begin
   index := GetSectionIndex(section);
   if index <> -1 then
   begin
      Inc(index);
      while (index < FFileBuffer.Count) and not IsSection(FFileBuffer[index]) and
            (GetName(FFileBuffer[index]) <> ident) do
         Inc(index);
      if not (index >= FFileBuffer.Count) and not IsSection(FFileBuffer[index]) then
      begin         { Ident does exists }
         FFileBuffer.Delete(index);
         SaveToFile;
      end;
   end;
end;

{ Erases the whole Section from an Ini-File }

procedure TIniFile.EraseSection(const section : string);
var
   index : integer;
begin
   index := GetSectionIndex(section);
   if index <> -1 then
   begin
      FFileBuffer.Delete(index);           { Delete Section-Header }
      while (index < FFileBuffer.Count) and not IsSection(FFileBuffer[index]) do
         FFileBuffer.Delete(index);        { Delete Section-Items }
      if index > 0 then FFileBuffer.Insert(index, '');
    SaveToFile;
  end;
end;

end.

{
  $Log$
  Revision 1.10  2000-04-08 17:59:47  michael
  + Searching of values is now case-insensitive

  Revision 1.9  2000/03/11 15:56:17  michael
  + Added EscapeLinefeeds boolean property.

  Revision 1.8  2000/01/07 01:24:33  peter
    * updated copyright to 2000

  Revision 1.7  2000/01/06 01:20:33  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:07  peter
    * moved to packages dir

  Revision 1.5  1999/11/23 09:50:51  peter
    * load/save stream support

  Revision 1.4  1999/11/08 15:01:38  peter
    * fpcmake support

  Revision 1.3  1999/11/02 23:58:37  peter
    * comment support
    * readsectionraw method

  Revision 1.2  1999/04/29 16:21:54  michael
  + Default mode Hugestrings

  Revision 1.1  1999/04/08 15:44:10  michael
  + Donated by Michael A. Hess

  Initial Release 1999/04/07 MAH

}