{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Test uriparser unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE objfpc}
{$H+}

program Testuri;

uses URIParser;

var
  URI: TURI;
  s: String;
begin
  with URI do
  begin
    Protocol := 'http';
    Username := 'user';
    Password := 'pass';
    Host := 'localhost';
    Port := 8080;
    Path := '/test/dir';
    Document := 'some index.html';
    Params := 'param1=value1&param2=value2';
    Bookmark := 'bookmark';
  end;

  s := EncodeURI(URI);
  WriteLn(s);

  FillChar(URI, SizeOf(URI), #0);

//  URI := ParseURI(s, 'defaultprotocol', 1234);
  URI:=ParseURI('http://www.lazarus.freepascal.org/main.php');
  with URI do
  begin
    WriteLn('Protocol: ', Protocol);
    WriteLn('Username: ', Username);
    WriteLn('Password: ', Password);
    WriteLn('Host: ', Host);
    WriteLn('Port: ', Port);
    WriteLn('Path: ', Path);
    WriteLn('Document: ', Document);
    WriteLn('Params: ', Params);
    WriteLn('Bookmark: ', Bookmark);
  end;

end.
