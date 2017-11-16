{ ********************************************************************* 
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2016 Michael Van Canneyt.
       
    Javascript minifier
            
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
                   
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
                                
  **********************************************************************}
{ ---------------------------------------------------------------------
  Javascript minifier, based on an implementation by Douglas Crockford,
  see original copyright.
  ---------------------------------------------------------------------}
{ jsmin.c
   2013-03-29

Copyright (c) 2002 Douglas Crockford  (www.crockford.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

The Software shall be used for Good, not Evil.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
}

unit jsminifier;

{$mode objfpc}{$H+}
{$inline on}

interface

uses sysutils,classes,bufstream;


Const
  EOS = #0;

Type

  { TJSONMinifier }
  EJSONMinifier = Class(Exception);

  TJSONMinifier = Class(TComponent)
  Private
    FA : char;
    FB : char;
    FFileHeader: TStrings;
    FLookahead : char;
    FX : char;
    FY : char ;
    Fin : TStream;
    Fout : TStream;
    procedure SetFileHeader(AValue: TStrings);
  Protected
    // Token reading routines
    function Peek : char;
    function Get : char;inline;
    function Next : char;
    // Token writing routines
    procedure Putc(c: char);inline;
    Procedure Reset;
    procedure DoHeader; virtual;
    procedure Error(Const Msg: string);
    Class Function isAlphaNum(c: char): boolean;
    Class Function iif(B : Boolean; Const ifTrue,ifFalse : integer) : integer; inline;
    procedure Action(d: Byte);
    procedure Minify;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Execute(Const SourceFilename,DestFilename : String);
    Procedure Execute(Source,Dest : TStream);
    Procedure Execute(SourceFilenames : TStrings; Const DestFilename : String);
    Procedure Execute(SourceFileNames : Array of string; Const DestFilename : String);
  Published
    Property FileHeader : TStrings Read FFileHeader Write SetFileHeader;
  end;

Implementation

Resourcestring
  SErrUnterminatedComment = 'Unterminated comment.';
  SErrUnterminatedStringLiteral = 'Unterminated string literal.';
  SErrUnterminatedSetInRegexp = 'Unterminated set in Regular Expression literal.';
  SerrUnterminatedRegexp = 'Unterminated Regular Expression literal.';

class function TJSONMinifier.iif(B: Boolean; const ifTrue, ifFalse: integer
  ): integer;

begin
  if B then
    Result:=ifTrue
  else
    Result:=ifFalse;
end;

procedure TJSONMinifier.Error(const Msg: string);

begin
  Raise EJSONMinifier.Create('JSMIN Error: '+Msg);
end;

procedure TJSONMinifier.SetFileHeader(AValue: TStrings);
begin
  if FFileHeader=AValue then Exit;
  FFileHeader.Assign(AValue);
end;

procedure TJSONMinifier.Reset;

begin
  FA:=EOS;
  FB:=EOS;
  FLookahead:=EOS;
  FX:=EOS;
  FY:=EOS;
end;

class function TJSONMinifier.isAlphaNum(c: char): boolean;

begin
  Result:= (C in ['a'..'z']) or (c in ['0'..'9']) or (c in ['A'..'Z']) or (C in ['_','$','\']) or (c > #126);
end;


function TJSONMinifier.Get: char;

begin
  Result:=FLookahead;
  FLookahead:=EOS;
  if (Result=EOS) then
    if Fin.Read(Result,sizeof(Result))=0 then exit;
  if (Result>' ') or (Result in [#10,EOS]) then
    Exit;
  if (Result=#13) then
    Result:=#10
  else
    Result:=' ';
end;


function TJSONMinifier.Peek: char;
begin
  FLookahead := get();
  result:=FLookahead;
end;

function TJSONMinifier.Next: char;

var
 c : char;

begin
  c:= get();
  if (c='/') then
    case peek of
      '/': Repeat
             c := get();
           until (c <= #10);
      '*':
           begin
           Get();
           while (c <> ' ') do
             case get of
               '*':
                 begin
                 if (peek()= '/') then
                   begin
                   get();
                   c:=' ';
                   end;
                 end;
               EOS:
                 Error(SErrUnterminatedComment);
              end;
           end;
    end;
  FY:=FX;
  FX:=c;
  Result:=c;
end;

procedure TJSONMinifier.Putc(c: char);

begin
  Fout.writebuffer(c,sizeof(c));
end;

procedure TJSONMinifier.Action(d : Byte);

  Procedure Do1;

  begin
    putc(FA);
    if ((FY in [#10,' '])
        and (FA in ['+','-','*','/'])
        and (FB in ['+','-','*','/'])) then
      putc(FY);
  end;

  Procedure Do2;

  begin
    FA:=FB;
    if (FA in ['''','"','`']) then
      While true do
        begin
        putc(FA);
        FA:= get();
        if (FA=FB) then
          break;
        if (FA='\') then
          begin
          putc(FA);
          FA:=get();
          end;
        if (FA=EOS) then
          Error(SErrUnterminatedStringLiteral);
        end;
  end;

begin
  if (D=1) then
    Do1;
  if (D in [1,2]) then
    Do2;
  FB := next();
  if (FB='/') and (FA in ['(',',','=',':','[','!','&','|','?','+','-','~','*','/','{',#10]) then
    begin
    putc(FA);
    if (FA in ['/','*']) then
       putc(' ');
    putc(FB);
    While true do
      begin
      FA := get();
      if (FA='[') then
        begin
        While true do
          begin
          putc(FA);
          FA := get();
          if (FA = ']') then
            break;
          if (FA = '\') then
            begin
            putc(FA);
            FA := get();
            end;
          if (FA = EOS) then
            Error(SErrUnterminatedSetInRegexp);
          end
        end
      else if (FA = '/') then
        begin
        case (peek()) of
           '/', '*':
            Error(SErrUnterminatedSetInRegexp);
        end;
        Break;
        end
      else if (FA ='\') then
        begin
        putc(FA);
        FA := get();
        end;
      if (FA = EOS) then
        Error(SErrUnterminatedRegexp);
      putc(FA);
      end;
    FB := next();
    end;
end;


procedure TJSONMinifier.Minify;

begin
  if (peek()= #$EF) then
    begin
    get();
    get();
    get();
    end;
  FA:=#10;
  action(3);
  while (FA <> EOS) do
    begin
    case (FA) of
      ' ':
        action(iif(isAlphanum(FB),1,2));
      #10:
        case (FB) of
          '{', '[', '(', '+', '-', '!', '~':
            Action(1);
          ' ':
                Action(3);
        else
          Action(iif(isAlphanum(FB), 1 , 2));
        end;
    else
      case (FB) of
        ' ':
          Action(iif(isAlphanum(FA),1,3));
        #10:
          case (FA) of
            '}',']',')','+','-','"', '''', '`':
              Action(1);
          else
            Action(iif(isAlphanum(FA), 1, 3));
         end;
      else
        Action(1);
      end;
    end;
    end;
end;

constructor TJSONMinifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFileHeader:=TStringList.Create;
end;

destructor TJSONMinifier.Destroy;
begin
  FreeAndNil(FFileHeader);
  inherited Destroy;
end;

procedure TJSONMinifier.Execute(const SourceFilename, DestFilename: String);

Var
 Src,Dest : TBufStream;

begin
 Dest:=Nil;
 Src:=TReadBufStream.Create(TFileStream.Create(SourceFileName,fmOpenRead or fmShareDenyWrite),1000);
 try
   Src.SourceOwner:=True;
   Dest:=TWriteBufStream.Create(TFileStream.create(DestFileName,fmCreate),1000);
   Dest.SourceOwner:=True;
   Execute(Src,Dest);
 finally
   Src.Free;
   Dest.Free;
 end;
end;

procedure TJSONMinifier.DoHeader;

Var
  S,L : String;

begin
  For S in FFileHeader do
    begin
    L:='// '+S+sLineBreak;
    Fout.WriteBuffer(L[1],Length(L));
    end;
end;

procedure TJSONMinifier.Execute(Source, Dest: TStream);

begin
  Fin:=Source;
  Fout:=Dest;
  try
    Reset;
    DoHeader;
    Minify;
  finally
    Fin:=Nil;
    Fout:=Nil;
  end;
end;

procedure TJSONMinifier.Execute(SourceFilenames: TStrings;const DestFilename: String);

Var
  Src,Dest : TBufStream;
  I : Integer;

begin
 Dest:=Src;
 Dest:=TWriteBufStream.Create(TFileStream.create(DestFileName,fmCreate),1000);
 try
   Dest.SourceOwner:=True;
   for I:=0 to SourceFileNames.Count-1 do
     begin
     Src:=TReadBufStream.Create(TFileStream.Create(SourceFileNames[i],fmOpenRead or fmShareDenyWrite),1000);
     Src.SourceOwner:=True;
     Execute(Src,Dest);
     FreeAndNil(Src);
     end;
 finally
   FreeAndNil(Src);
   FreeAndNil(Dest);
 end;
end;

procedure TJSONMinifier.Execute(SourceFileNames: array of string;
  const DestFilename: String);

Var
  S : TStrings;

begin
  S:=TStringList.Create;
  try
    S.AddStrings(SourceFileNames);
    Execute(S,DestFileName);
  finally
    S.Free;
  end;
end;


end.

