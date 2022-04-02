{ Demo for CSS engine : CGI to minimize a CSS file or extract class names

  Copyright (C) 2022- michael Van Canneyt michael@freepascal.org

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as
  published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

  A copy of the GNU General Public License is available on the World Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can
  also obtain it by writing to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

program fpcss;

{ $DEFINE USEHTTPAPP}

uses
  SysUtils, classes, fpcssutils,
  {$IFDEF USEHTTPAPP} fphttpapp{$ELSE} fpcgi {$ENDIF},
  httpdefs, httproute;

Function GetCSS(aRequest : TRequest) : TStream;

begin
  Result:=TStringStream.Create(aRequest.Content);
end;

procedure DoExtract(ARequest: TRequest; AResponse: TResponse);

Var
  S : TStream;
  aList : TStrings;
  Utils : TCSSUtils;

begin  
  S:=Nil;  
  aList:=Nil;
  Utils:=TCSSUtils.Create(Nil);
  try
    S:=GetCSS(aRequest);
    aList:=TstringList.Create;
    Utils.ExtractClassNames(S,aList);
    aResponse.ContentLength:=Length(aResponse.Content);
    aResponse.ContentType:='text/text';
    aResponse.Content:=aList.Text;
    aResponse.SendResponse;
  finally
    aList.Free;
    Utils.Free;
    S.Free;
  end;
end;


procedure DoMinimize(ARequest: TRequest; AResponse: TResponse);

Var
  Sin,SOut : TStream;
  Utils : TCSSUtils;

begin  
  Sin:=Nil;  
  Sout:=Nil;
  Utils:=TCSSUtils.Create(Nil);
  try
    Sin:=GetCSS(aRequest);
    SOut:=TStringStream.Create;
    Utils.Minimize(Sin,Sout);
    aResponse.ContentLength:=Length(aResponse.Content);
    aResponse.ContentType:='text/text';
    aResponse.ContentStream:=SOut;
    aResponse.ContentLength:=Sout.Size;
    aResponse.SendResponse;
  finally
    Sout.Free;
    Utils.Free;
    Sin.Free;
  end;
end;


begin
  HTTPRouter.RegisterRoute('minimize',rmPost,@DoMinimize);
  HTTPRouter.RegisterRoute('classnames',rmPost,@DoExtract);
  {$IFDEF USEHTTPAPP}
  Application.Port:=8080;
  {$ENDIF}
  Application.Title:='CSS utils CGI';
  Application.Initialize;
  Application.Run;
end.

