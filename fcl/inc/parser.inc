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
{*                             TParser                                      *}
{****************************************************************************}

Procedure TParser.ReadBuffer;

begin
end;


Procedure TParser.SkipBlanks;

begin
end;


constructor TParser.Create(Stream: TStream);

begin
end;


destructor TParser.Destroy;

begin
end;


Procedure TParser.CheckToken(T: Char);

begin
end;


Procedure TParser.CheckTokenSymbol(const S: string);

begin
end;


Procedure TParser.Error(const Ident: string);

begin
end;


{!!!!!!
Procedure TParser.ErrorFmt(const Ident: string; const Args: array of const);

begin
end;
!!!!!!}

Procedure TParser.ErrorStr(const Message: string);

begin
end;


Procedure TParser.HexToBinary(Stream: TStream);

begin
end;


Function TParser.NextToken: Char;

begin
end;


Function TParser.SourcePos: Longint;

begin
end;


Function TParser.TokenComponentIdent: String;

begin
end;


Function TParser.TokenFloat: Extended;

begin
end;


Function TParser.TokenInt: Longint;

begin
end;


Function TParser.TokenString: string;

begin
end;


Function TParser.TokenSymbolIs(const S: string): Boolean;

begin
end;
{
  $Log$
  Revision 1.1  1998-05-04 14:30:12  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
