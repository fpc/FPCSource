{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    TCGIApplication class.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
{$mode objfpc}
{$H+}

unit cgiapp;

Interface

uses
  CustApp,Classes,SysUtils;

Const
  CGIVarCount = 23;

Type  
  TCGIVarArray = Array[1..CGIVarCount] of String;
  
Const   
  CgiVarNames : TCGIVarArray = 
   ('AUTH_TYPE',
    'CONTENT_LENGTH',
    'CONTENT_TYPE', 
    'GATEWAY_INTERFACE', 
    'PATH_INFO', 
    'PATH_TRANSLATED',
    'QUERY_STRING', 'REMOTE_ADDR',
    'REMOTE_HOST',
    'REMOTE_IDENT',
    'REMOTE_USER',
    'REQUEST_METHOD',
    'SCRIPT_NAME',
    'SERVER_NAME',
    'SERVER_PORT',
    'SERVER_PROTOCOL',
    'SERVER_SOFTWARE',
    'HTTP_ACCEPT',
    'HTTP_ACCEPT_CHARSET',
    'HTTP_ACCEPT_ENCODING',
    'HTTP_IF_MODIFIED_SINCE',
    'HTTP_REFERER',
    'HTTP_USER_AGENT');

Type 
  
  TCgiApplication = Class(TCustomApplication)
  Private
    FEmail : String;
    FAdministrator : String;
    FContentTypeEmitted : Boolean;
    FCGIVars : TCGIVarArray;
    FRequestVars : TStrings;
    Function GetCGIVar (Index : Integer) : String;
    Procedure InitCGIVars;
    Procedure InitRequestVars;
    Procedure InitPostVars;
    Procedure InitGetVars;
    Procedure SetContentLength (Value : Integer);
    Procedure SetCGIVar(Index : Integer; Value : String);
    Function GetContentLength : Integer;
    Function GetServerPort : Word;
    Function GetEmail : String;
    Function GetAdministrator : String;
    Procedure ProcessQueryString(Const FQueryString : String);
    Function GetRequestVariable(Const VarName : String) : String;
    Function GetRequestVariableCount : Integer;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Initialize; override;
    Procedure GetCGIVarList(List : TStrings);
    Procedure GetRequestVarList(List : TStrings);
    Procedure GetRequestVarList(List : TStrings; NamesOnly : Boolean);
    Procedure ShowException(E: Exception);override;
    Function EmitContentType : Boolean;
    Property AuthType : String Index 1 Read GetCGIVar;
    Property ContentLength : Integer Read GetContentLength Write SetContentLength; // Index 2
    Property ContentType : String Index 3 Read GetCGIVar Write SetCGIVar;
    Property GatewayInterface : String Index 4 Read GetCGIVar;
    Property PathInfo : String index 5 read GetCGIvar;
    Property PathTranslated : String Index 6 read getCGIVar;
    Property QueryString : String Index 7 read getcgivar;
    Property RemoteAddress : String Index 8 read GetCGIVar;
    Property RemoteHost : String Index 9 read GetCGIVar;
    Property RemoteIdent : String Index 10 read GetCGIVar;
    Property RemoteUser : String Index 11 read GetCGIVar;
    Property RequestMethod : String Index 12 read GetCGIVar;
    Property ScriptName : String Index 13 read GetCGIVar;
    Property ServerName : String Index 14 read GetCGIVar;
    Property ServerPort : Word Read GetServerPort; // Index 15
    Property ServerProtocol : String Index 16 read GetCGIVar; 
    Property ServerSoftware : String Index 17 read GetCGIVar;
    Property HTTPAccept : String Index 18 read GetCGIVar;
    Property HTTPAcceptCharset : String Index 19 read GetCGIVar;
    Property HTTPAcceptEncoding : String Index 20 read GetCGIVar;
    Property HTTPIfModifiedSince : String Index 21 read GetCGIVar; // Maybe change to TDateTime ??
    Property HTTPReferer : String Index 22 read GetCGIVar;
    Property HTTPUserAgent : String Index 23 read GetCGIVar;
    Property Email : String Read GetEmail Write FEmail;
    Property Administrator : String Read GetAdministrator Write FAdministrator;
    Property RequestVariables[VarName : String] : String Read GetRequestVariable;
    Property RequestVariableCount : Integer Read GetRequestVariableCount;
  end;

ResourceString
  SWebMaster = 'webmaster';  
  SCGIError  = 'CGI Error';
  SAppEncounteredError = 'The application encountered the following error:';
  SError     = 'Error: ';
  SNotify    = 'Notify: ';
  SErrNoContentLength = 'No content length passed from server!';
  SErrUnsupportedContentType = 'Unsupported content type: "%s"';
  SErrNoRequestMethod = 'No REQUEST_METHOD passed from server.';
  SErrInvalidRequestMethod = 'Invalid REQUEST_METHOD passed from server.';
  
Implementation

Constructor TCgiApplication.Create(AOwner : TComponent);

begin
  Inherited Create(AOwner);
  FRequestVars:=TStringList.Create;
end;

Destructor TCgiApplication.Destroy;

begin
  FRequestVars.Free;
  Inherited;
end;

Function  TCgiApplication.GetCGIVar (Index : Integer) : String;

begin
  Result:=FCGIVars[Index];
end;

Procedure TCgiApplication.InitCGIVars;

Var
  I : Integer;
  L : TStrings;
  
begin
  L:=TStringList.Create;
  Try
    GetEnvironmentList(L);
    For I:=1 to CGIVarCount do
      FCGIVars[i]:=L.Values[CGIVarNames[i]];
  Finally
    L.Free;
  end;  
end;

Procedure TCgiApplication.Initialize; 

begin
  StopOnException:=True;
  Inherited;
  InitCGIVars;
  InitRequestVars;
end;

Procedure TCgiApplication.GetCGIVarList(List : TStrings);

Var
  I : Integer;
  
begin
  List.Clear;
  For I:=1 to cgiVarCount do
    List.Add(CGIVarNames[i]+'='+FCGIVars[i]);
end;

Procedure TCgiApplication.GetRequestVarList(List : TStrings);

begin
  GetRequestVarList(List,False);
end;

Procedure TCgiApplication.GetRequestVarList(List : TStrings; NamesOnly : Boolean);

Var
  I,J : Integer;
  S : String;
  
begin
  List.BeginUpdate;
  Try
    List.Clear;
    // Copy one by one, there may be CR/LF in the variables, causing 'Text' to go wrong.
    If Assigned(FRequestVars) then
      For I:=0 to FRequestVars.Count-1 do
        begin
        S:=FRequestVars[i];
        If NamesOnly then
          begin
          J:=Pos('=',S);
          If (J>0) then
            S:=Copy(S,1,J-1);
          end;
        List.Add(S);
        end;
  finally 
    List.EndUpdate;
  end;
end;


Function TCgiApplication.GetContentLength : Integer;

begin
  Result:=StrToIntDef(GetCGIVar(2),-1);
end;

Procedure TCgiApplication.SetContentLength (Value : Integer);

begin
  SetCGIVar(2,IntToStr(Value));
end;

Procedure TCgiApplication.SetCGIVar(Index : Integer; Value : String);

begin
  If Index in [1..cgiVarCount] then
    FCGIVars[Index]:=Value;
end;


Function TCgiApplication.GetServerPort : Word;
begin
  Result:=StrToIntDef(GetCGIVar(15),0);
end;

Function TCgiApplication.EmitContentType : Boolean;

Var
  S: String;
  
begin
  Result:=Not FContentTypeEmitted;
  If result then
    begin
    S:=ContentType;
    If (S='') then
      S:='text/html';
    writeln('Content-Type: ',ContentType);
    writeln;
    FContentTypeEmitted:=True;
    end;
end;

Procedure TCgiApplication.ShowException(E: Exception);

Var
  TheEmail : String;

begin
  If not FContentTypeEmitted then
    begin
    ContentType:='text/html';
    EmitContentType;
    end;
  If (ContentType='text/html') then
    begin
    writeln('<html><head><title>',Title,SCGIError,'</title></head>');
    writeln('<body>');
    writeln('<center><hr><h1>',Title,': ERROR</h1><hr></center><br><br>');
    writeln(SAppEncounteredError ,'<br>');
    writeln('<ul>');
    writeln('<li>',SError,' <b>',E.Message,'</b></ul><hr>');
    TheEmail:=Email;
    If (TheEmail<>'') then
      writeln('<h5><p><i>',SNotify,Administrator,': <a href="mailto:',TheEmail,'">',TheEmail,'</a></i></p></h5>');
    writeln('</body></html>');
    end;
end;

Function TCgiApplication.GetEmail : String;

Var
  H : String;
  
begin
  If (FEmail='') then
    begin
    H:=ServerName;
    If (H<>'') then
      Result:=Administrator+'@'+H
    else
      Result:='';  
    end
  else
    Result:=Email;  
end;

Function TCgiApplication.GetAdministrator : String;

begin
  If (FADministrator<>'') then
    Result:=FAdministrator
  else
    Result:=SWebMaster;
end;

Procedure TCgiApplication.InitRequestVars;

var
  R : String;
   
begin
  R:=RequestMethod;
  if (R='') then
    Raise Exception.Create(SErrNoRequestMethod);
  if CompareText(R,'POST')=0 then
    InitPostVars
  else if CompareText(R,'GET')=0 then
    InitGetVars
  else
    Raise Exception.CreateFmt(SErrInvalidRequestMethod,[R]);
end;

Procedure TCgiApplication.InitPostVars;

var
  FQueryString : String;
  i : Integer;
  ch : Char;

begin
  if (FCGIVars[2]='') then
    Raise Exception.Create(SErrNoContentLength);
  if CompareText(ContentType,'APPLICATION/X-WWW-FORM-URLENCODED')<>0 then
    Raise Exception.CreateFmt(SErrUnsupportedContentType,[ContentType]);
  SetLength(FQueryString,ContentLength);
  for I:= 1 to contentLength Do
    begin
    Read(ch);
    FQueryString[i]:=ch;
    end;
  ProcessQueryString(FQueryString);
end;

Procedure TCgiApplication.InitGetVars;

Var
  FQueryString : String;

begin
  FQueryString:=QueryString;
  If (FQueryString<>'') then
    ProcessQueryString(FQueryString);
end;

const
   hexTable = '0123456789ABCDEF';
   
Procedure TCgiApplication.ProcessQueryString(Const FQueryString : String);


var
  queryItem : String;
  delimiter : Char;
  aString : String;
  aSepStr : String;
  aPos    : Integer;
  aLenStr : Integer;
  aLenSep : Integer;

  function hexConverter(h1, h2 : Char) : Char;

  var
    B : Byte;
    
  begin
    B:=(Pos(upcase(h1),hexTable)-1)*16;
    B:=B+Pos(upcase(h2),hexTable)-1;
    Result:=chr(B);
  end;

  procedure Convert_ESC_Chars;

  var
    index : Integer;

  begin
    For Index:=1 to Length(QueryItem) do
    Index:=Length(QueryItem);    
    While (Index>0) do
      begin
      If QueryItem[Index]='+' then
        QueryItem[Index]:=' '
      else If (QueryItem[Index]='%') and (Index<Length(QueryItem)-1) then
        begin
        QueryItem[Index]:=hexConverter(QueryItem[Index+1],QueryItem[index+2]);
        System.Delete(QueryItem,Index+1,2);
        end;
      dec(Index);  
      end;
  end;

  procedure InitToken(aStr, aSep : String);
  
  begin
    aString := aStr;
    aSepStr := aSep;
    aPos    := 1;
    aLenStr := Length(aString);
    aLenSep := Length(aSepStr);
  end;

  function NextToken(var aToken : String; var aSepChar : Char) : Boolean;

  var
    i : Integer;
    j : Integer;
    BoT : Integer;
    EoT : Integer;
    isSep : Boolean;
 
  begin
    BoT:=aPos;
    EoT:=aPos;
    for i:=aPos to aLenStr do
      begin
      IsSep := false;
      for j := 1 to aLenSep do
        begin
        if aString[i] = aSepStr[j] then
          begin
          IsSep := true;
          Break;
          end;
        end;
      if IsSep then
        begin
        EoT  := i;
        aPos := i + 1;
        aSepChar := aString[i];
        Break;
        end
      else
        begin
        if i = aLenStr then
          begin
          EoT  := i;
          aPos := i;
          Break;
          end;
        end;
      end;
    if aPos < aLenStr then
      begin
      aToken := Copy(aString, BoT, EoT - BoT);
      Result := true;
      end
    else
      begin
      if aPos = aLenStr then
        begin
        aToken := Copy(aString, BoT, EoT - BoT + 1);
        Result := true;
        aPos   := aPos + 1;
        end
      else
        begin
        Result := false;
       end;
    end;
  end;


begin
   InitToken(FQueryString, '&');
   while NextToken(QueryItem, delimiter) do
     begin
     if (QueryItem<>'') then
       begin
       Convert_ESC_Chars;
       FRequestVars.Add(QueryItem);
       end;
     end;
end;

Function TCGIApplication.GetRequestVariable(Const VarName : String) : String;

begin
 If Assigned(FRequestVars) then
   Result:=FRequestVars.Values[VarName];
end;

Function TCGIApplication.GetRequestVariableCount : Integer;

begin
  If Assigned(FRequestVars) then
    Result:=FRequestVars.Count
  else
    Result:=0;
end;

end.