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
program TestUri;

{$IFDEF FPC}
{$MODE OBJFPC}{$H+}
{$ENDIF}

uses
  uriparser;

var
  URI: TURI;
  s: String;

procedure TestParse;
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
	HasAuthority := True;
  end;

  s := EncodeURI(URI);
  WriteLn(s);

  Finalize(URI);
  FillChar(URI, SizeOf(URI), #0);
  Writeln;

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
end;  

type
  urirec = record
    a, b: string
  end;

const
  Base = 'http://a/b/c/d;p?q';

  tests: array[0..22] of urirec = (
    (a: 'g:h'; b: 'g:h'),
    (a: 'g';   b: 'http://a/b/c/g'),
    (a: './g'; b: 'http://a/b/c/g'),
    (a: 'g/';  b: 'http://a/b/c/g/'),
    (a: '/g';  b: 'http://a/g'),
    (a: '//g'; b: 'http://g'),
    (a: '?y';  b: 'http://a/b/c/d;p?y'),
    (a: 'g?y'; b: 'http://a/b/c/g?y'),
    (a: '#s';  b: 'http://a/b/c/d;p?q#s'),
    (a: 'g#s'; b: 'http://a/b/c/g#s'),
    (a: 'g?y#s'; b: 'http://a/b/c/g?y#s'),
    (a: ';x';  b: 'http://a/b/c/;x'),
    (a: 'g;x'; b: 'http://a/b/c/g;x'),
    (a: 'g;x?y#s'; b: 'http://a/b/c/g;x?y#s'),
    (a: '';    b: 'http://a/b/c/d;p?q'),
    (a: '.';   b: 'http://a/b/c/'),
    (a: './';  b: 'http://a/b/c/'),
    (a: '..';  b: 'http://a/b/'),
    (a: '../'; b: 'http://a/b/'),
    (a: '../g'; b: 'http://a/b/g'),
    (a: '../..'; b: 'http://a/'),
    (a: '../../'; b: 'http://a/'),
    (a: '../../g'; b: 'http://a/g')
  );

  tests1: array[0..1] of urirec = (
    (a: '../../../g';    b: 'http://a/g'),
    (a: '../../../../g'; b: 'http://a/g')
  );

  tests2: array[0..5] of urirec = (
    (a: '/./g';  b: 'http://a/g'),
    (a: '/../g'; b: 'http://a/g'),
    (a: 'g.';    b: 'http://a/b/c/g.'),
    (a: '.g';    b: 'http://a/b/c/.g'),
    (a: 'g..';   b: 'http://a/b/c/g..'),
    (a: '..g';   b: 'http://a/b/c/..g')
  );

  tests3: array[0..5] of urirec = (
    (a: './../g'; b: 'http://a/b/g'),
    (a: './g/.';  b: 'http://a/b/c/g/'),
    (a: 'g/./h';  b: 'http://a/b/c/g/h'),
    (a: 'g/../h'; b: 'http://a/b/c/h'),
    (a: 'g;x=1/./y';  b: 'http://a/b/c/g;x=1/y'),
    (a: 'g;x=1/../y'; b: 'http://a/b/c/y')
  );

  tests4: array[0..3] of urirec = (
    (a: 'g?y/./x';  b: 'http://a/b/c/g?y/./x'),
    (a: 'g?y/../x'; b: 'http://a/b/c/g?y/../x'),
    (a: 'g#s/./x';  b: 'http://a/b/c/g#s/./x'),
    (a: 'g#s/../x'; b: 'http://a/b/c/g#s/../x')
  );

procedure Test(const Caption: string; const t: array of urirec);
var
  rslt: UTF8String;
  i: Integer;
  Failed: Boolean;
begin
  write(Caption, '...');
  Failed := False;
  for i := low(t) to high(t) do
  begin
    ResolveRelativeUri(Base, t[i].a, rslt);
    if rslt <> t[i].b then
    begin
      if not Failed then writeln;
      Failed := True;
      writeln('Test ', i, ' mismatch, expected: ''', t[i].b, '''; got: ''', rslt, '''');
    end;
  end;
  if not Failed then
    writeln(' OK');
end;

begin
  TestParse;
  Writeln;
  Writeln('Now testing relative URI resolving:');
  Test('Normal tests', tests);
  Test('URI authority is not changed by using dot segments', tests1);
  Test('Dot segments are removed only if they are complete path components', tests2);
  Test('Testing some nonsensical forms of URI', tests3);
  Test('Testing dot segments present in query or fragments', tests4);
end.
