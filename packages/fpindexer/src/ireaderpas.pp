{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2012 by the Free Pascal development team

    Pascal text reader
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit IReaderPAS;

{$mode objfpc}{$H+}

interface

uses
  Classes, fpIndexer, IReaderTXT;

type

  { TIReaderPAS }

  TIReaderPAS = class(TIReaderTXT)
  private
  protected
    function AllowedToken(token: string): boolean; override;
  public
    procedure LoadFromStream(FileStream: TStream); override;
  end;

implementation

{ TIReaderPAS }

function TIReaderPAS.AllowedToken(token: string): boolean;
begin
  Result:=inherited AllowedToken(token);
end;

procedure TIReaderPAS.LoadFromStream(FileStream: TStream);
begin
  inherited LoadFromStream(FileStream);
end;

initialization
  FileHandlers.RegisterFileReader('Pascal format', 'pas', TIReaderPAS);

end.

