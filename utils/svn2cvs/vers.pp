{$mode objfpc}
{$h+}
program vers;

uses Classes,sysutils,process,DOM,xmlread,custapp,IniFiles;

Type
 { TVersion }
 TVersion = Class(TCollectionItem)
 private
   FAuthor: String;
   FDate: string;
   FLogMessage: String;
   FRevision: Integer;
 Public
   Property Revision : Integer read FRevision;
   Property LogMessage : String Read FLogMessage;
   Property Date : string Read FDate;
   Property Author : String Read FAuthor;
 end;
 
 { TVersions }

 TVersions = Class(TCollection)
 private
   function GetVersion(Index : INteger): TVersion;
   procedure SetVersion(Index : INteger; const AValue: TVersion);
 Protected
   procedure ConvertLogEntry(E : TDomElement);
 public
   Procedure LoadFromXML(Doc : TXMlDocument);
   property Versions[Index : INteger] : TVersion Read GetVersion Write SetVersion; Default;
 end;

 AppError = Class(Exception); 

Resourcestring
  SErrInValidSVNLog = 'INvalid SVN log';

{ TVersions }

function TVersions.GetVersion(Index : INteger): TVersion;
begin
  Result:=Items[Index] as Tversion;
end;

procedure TVersions.SetVersion(Index : INteger; const AValue: TVersion);
begin
  Items[Index]:=AValue;
end;

procedure TVersions.ConvertLogEntry(E : TDomElement);

  Function GetNodeText(N : TDomNode) : String;
  
  begin
    N:=N.FirstChild;
    If N<>Nil then
      Result:=N.NodeValue;
  end;

Var
  N : TDomNode;
  V : TVersion;

begin
  V:=Add as TVersion;
  V.FRevision:=StrToIntDef(E['revision'],-1);
  N:=E.FirstChild;
  While (N<>Nil) do
    begin
    If (N.NodeType=ELEMENT_NODE) then
      begin
      if (N.NodeName='author') then
        V.FAuthor:=GetNodeText(N)
      else If (N.NodeName='date') then
        V.FDate:=GetNodeText(N)
      else If (N.NodeName='msg') then
        V.FLogMessage:=GetNodeText(N);
      end;
    N:=N.NextSibling;
    end;
end;

procedure TVersions.LoadFromXML(Doc: TXMlDocument);

var
  L : TDomNode;
  E : TDomElement;

begin
  L:=Doc.FirstChild;
  While (L<>Nil) and not ((L.NodeType=ELEMENT_NODE) and (L.NodeName='log')) do
    L:=L.NextSibling;
  if (L=Nil) then
    Raise AppError.Create(SErrInValidSVNLog);
  L:=L.FirstChild;
  While (L<>Nil) do
    begin
    If (L.NodeType=ELEMENT_NODE) and (L.NodeName='logentry') then
      E:=TDomElement(L);
    ConvertLogEntry(E);
    L:=L.NextSibling;
    end;
end;

Var
  Doc : TXMLDocument;
  F : TFileStream;
  I : Integer;

begin
  With TVersions.Create(TVersion) do 
    Try
      F:=TFileStream.Create('test.xml',fmOpenRead);
      Try
        ReadXMLFile(Doc,F);
        Writeln('Got ',Count,' revisions');
        LoadFromXml(Doc);
        For I:=0 to count-1 do
          begin
          Writeln('Revision ',I,' : ');
          Writeln('Revision : ',Versions[i].Revision);
          Writeln('Author   : ',Versions[i].Author);
          Writeln('Date     : ',Versions[i].Date);
          Writeln('Message  : ',Versions[i].LogMessage); 
          end;
      finally
        F.Free;
      end;  
    Finally
      Free;
    end;
end.
