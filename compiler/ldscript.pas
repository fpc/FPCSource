{
    Copyright (c) 2012 by Sergei Gorelkin

    A basic lexer for GNU ld scripts

    This program is free software; you can redistribute it and/or modify
    iu under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge- MA 02139, USA.

 ****************************************************************************
}

unit ldscript;

{$i fpcdefs.inc}

interface

uses
  owbase;

type
  TldScriptToken=char;

  TScriptLexer=class(TObject)
    data: ansistring;
    curtoken: TldScriptToken;
    curtokenstr: string;
    curpos: longint;
    line: longint;
    linestart: longint;
  public
    constructor Create(aReader:TObjectReader);
    procedure nextToken;
    function CheckForIdent(const s:string):boolean;
    function CheckFor(c:TldScriptToken):boolean;
    procedure Expect(c:TldScriptToken);
    property token:TldScriptToken read curtoken;
    property tokenstr:string read curtokenstr;
  end;

const
  tkEOF      = #0;
  tkINVALID  = #1;
  tkIDENT    = #2;
  tkNUMBER   = #3;
  tkLITERAL  = #4;

  tkLSHIFT   = #5;   { << }
  tkLE       = #6;   { <= }
  tkRSHIFT   = #7;   { >> }
  tkGE       = #8;   { >= }
  tkANDAND   = #9;   { && }
  tkANDEQ    = #10;   { &= }
  tkOROR     = #11;   { || }
  tkOREQ     = #12;   { |= }
  tkDIVEQ    = #13;   { /= }
  tkMULTEQ   = #14;   { *= }
  tkMINUSEQ  = #15;   { -= }
  tkPLUSEQ   = #16;   { += }
  tkNE       = #17;   { != }
  tkEQ       = #18;   { == }

  tkRSHIFTEQ = #19;   { >>= }
  tkLSHIFTEQ = #20;   { <<= }

implementation

uses
  sysutils;

const
  NameChars=['A'..'Z','a'..'z','_','.','$','0'..'9','+','-','=',',','*','?','/','~','\','[',']'];


{*****************************************************************************
                               TSCRIPTLEXER
*****************************************************************************}

constructor TScriptLexer.Create(AReader:TObjectReader);
begin
  { Expected data size is few hundred bytes,  }
  SetLength(data,AReader.size);
  AReader.Read(data[1],AReader.size);
  curpos:=1;
end;

procedure TScriptLexer.nextToken;
  var
    p,start: longint;
  begin
    p:=curpos;
    repeat
      { skip whitespace }
      while (data[p] in [#32,#9,#13]) do
        inc(p);
      start:=p;
      { C-style comment }
      if (data[p]='/') and (data[p+1]='*') then
        begin
          inc(p,2);
          while (data[p]<>'*') and (data[p+1]<>'/') do
            begin
              if (data[p]=#0) then
                begin
                  curtoken:=tkINVALID;
                  exit;
                end;
              if (data[p]=#10) then
                begin
                  inc(line);
                  linestart:=p+1;
                end;
              inc(p);
            end;
          inc(p,2);
          continue;
        end
      else if (data[p]=#10) then
        begin
          inc(p);
          inc(line);
          linestart:=p;
          continue;
        end
      else if (data[p]='#') then  { line comment }
        begin
          inc(p);
          while (data[p]<>#0) and (data[p]<>#10) do
            inc(p);
          continue;
        end;

      case data[p] of
        #0: curtoken:=tkEOF;

        '/':
          if (data[p+1] in NameChars) then
            begin
              inc(p);
              while (data[p] in NameChars) do
                inc(p);
              curtoken:=tkIDENT;
            end
          else if (data[p+1]='=') then
            curtoken:=tkDIVEQ
          else
            curtoken:='/';

        'A'..'Z','a'..'z','_','.','$','\':
          begin
            inc(p);
            while (data[p] in NameChars) do
              inc(p);
            curtoken:=tkIDENT;
          end;

        '0'..'9':
          begin
            if (data[p]='0') and (data[p+1] in ['x','X']) then
              begin
                inc(p,2);
                while data[p] in ['0'..'9','a'..'f','A'..'F'] do
                  inc(p);
              end
            else
              while (data[p] in ['0'..'9']) do
                inc(p);
            curtoken:=tkNUMBER;
          end;

        '"':
          begin
            inc(p);
            while (data[p]<>'"') and (data[p]<>#10) do
              inc(p);
            if data[p]=#10 then
              begin
                curtoken:=tkINVALID;
                exit;
              end;
            inc(p);
            curtoken:=tkLITERAL;
          end;

        '<':
          if (data[p+1]='<') then
            begin
              if (data[p+2]='=') then
                curtoken:=tkLSHIFTEQ
              else
                curtoken:=tkLSHIFT;
            end
          else if (data[p+1]='=') then
            curtoken:=tkLE
          else
            curtoken:='<';

        '>':
          if (data[p+1]='>') then
            begin
              if (data[p+2]='=') then
                curtoken:=tkRSHIFTEQ
              else
                curtoken:=tkRSHIFT;
            end
          else if (data[p+1]='=') then
            curtoken:=tkGE
          else
            curtoken:='>';

        '!':
          if (data[p+1]='=') then
            curtoken:=tkNE
          else
            curtoken:='!';

        '&':
          if (data[p+1]='&') then
            curtoken:=tkANDAND
          else if (data[p+1]='=') then
            curtoken:=tkANDEQ
          else
            curtoken:='&';

        '|':
          if (data[p+1]='|') then
            curtoken:=tkOROR
          else if (data[p+1]='=') then
            curtoken:=tkOREQ
          else
            curtoken:='|';

        '*':
          if (data[p+1]='=') then
            curtoken:=tkMULTEQ
          else
            curtoken:='*';

        '+':
          if (data[p+1]='=') then
            curtoken:=tkPLUSEQ
          else
            curtoken:='+';

        '-':
          if (data[p+1]='=') then
            curtoken:=tkMINUSEQ
          else
            curtoken:='-';

        '=':
          if (data[p+1]='=') then
            curtoken:=tkEQ
          else
            curtoken:='=';

        '(',')','{','}','[',']',';','?',':':
          curtoken:=data[p];
      else
        curtoken:=tkINVALID;
        exit;
      end;
      break;
    until false;
    case curtoken of
      tkRSHIFTEQ,tkLSHIFTEQ: inc(p,3);
      tkLSHIFT..tkEQ: inc(p,2);
      #32..#255: inc(p);
      tkIDENT,tkNUMBER:
        setstring(curtokenstr,@data[start],p-start);
      tkLITERAL:
        setstring(curtokenstr,@data[start+1],p-start-2);
    end;
    curpos:=p;
  end;

procedure TScriptLexer.Expect(c:TldScriptToken);
  begin
    if (curtoken=c) then
      nextToken
    else
      {error};
  end;

function TScriptLexer.CheckForIdent(const s:string):boolean;
  begin
    result:=(curtoken=tkIDENT) and (curtokenstr=s);
    if result then
      nextToken;
  end;

function TScriptLexer.CheckFor(c:TldScriptToken):boolean;
  begin
    result:=(curtoken=c);
    if result then
      nextToken;
  end;

end.

