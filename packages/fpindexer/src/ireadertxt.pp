{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2012 by the Free Pascal development team

    Plain text reader
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit IReaderTXT;

{$mode objfpc}{$H+}

interface

uses
  Classes, fpIndexer;

type

  { TIReaderTXT }

  TIReaderTXT = class(TCustomFileReader)
  private
  protected
    function AllowedToken(token: string): boolean; override;
  public
    procedure LoadFromStream(FileStream: TStream); override;
  end;

implementation

{ TIReaderTXT }

function TIReaderTXT.AllowedToken(token: string): boolean;
begin
  Result := inherited AllowedToken(token) and (Length(token) > 1);
end;

procedure TIReaderTXT.LoadFromStream(FileStream: TStream);
var
  token: string;
  p: TSearchWordData;
begin
  inherited LoadFromStream(FileStream);
  token := GetToken;
  while token <> '' do
  begin
    if AllowedToken(token) then
    begin
      p.SearchWord := token;
      P.Position:=TokenStartPos;
      p.Context:=GetContext;
      Add(p);
    end;
    token := GetToken;
  end;
end;

initialization
  FileHandlers.RegisterFileReader('Text format', 'txt', TIReaderTXT);

end.

