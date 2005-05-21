{
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit WHTML;

{$I globdir.inc}

interface

uses Objects;

type
    PTextFile = ^TTextFile;
    TTextFile = object(TObject)
      function GetLine(Idx: sw_integer; var S: string): boolean; virtual;
    end;

    PMemoryTextFile = ^TMemoryTextFile;
    TMemoryTextFile = object(TTextFile)
      constructor Init;
      procedure   AddLine(const S: string); virtual;
      function    GetLine(Idx: sw_integer; var S: string): boolean; virtual;
      destructor  Done; virtual;
    private
      Lines : PUnsortedStrCollection;
    end;

    PDOSTextFile = ^TDOSTextFile;
    TDOSTextFile = object(TMemoryTextFile)
      constructor Init(AFileName: string);
    end;

    PSGMLParser = ^TSGMLParser;
    TSGMLParser = object(TObject)
      constructor Init;
      function    Process(HTMLFile: PTextFile): boolean; virtual;
      function    ProcessLine(LineText: string): boolean; virtual;
      destructor  Done; virtual;
    public
      Line,LinePos: sw_integer;
      procedure   DocSoftBreak; virtual;
      function    DocAddTextChar(C: char): boolean; virtual;
      procedure   DocAddText(S: string); virtual;
      procedure   DocProcessTag(Tag: string); virtual;
      procedure   DocProcessComment(Comment: string); virtual;
      function    DocDecodeNamedEntity(Name: string; var Entity: string): boolean; virtual;
    private
      CurTag: string;
      InTag,InComment,InString: boolean;
    end;

    PHTMLParser = ^THTMLParser;
    THTMLParser = object(TSGMLParser)
      procedure   DocSoftBreak; virtual;
      function    DocAddTextChar(C: char): boolean; virtual;
      procedure   DocProcessTag(Tag: string); virtual;
      function    DocGetTagParam(Name: string; var Value: string): boolean; virtual;
      procedure   DocProcessComment(Comment: string); virtual;
      function    DocDecodeNamedEntity(Name: string; var E: string): boolean; virtual;
    public
      TagName,TagParams: string;
      procedure   DocUnknownTag; virtual;
      procedure   DocTYPE; virtual;
      procedure   DocHTML(Entered: boolean); virtual;
      procedure   DocHEAD(Entered: boolean); virtual;
      procedure   DocMETA; virtual;
      procedure   DocTITLE(Entered: boolean); virtual;
      procedure   DocBODY(Entered: boolean); virtual;
      procedure   DocAnchor(Entered: boolean); virtual;
      procedure   DocHeading(Level: integer; Entered: boolean); virtual;
      procedure   DocParagraph(Entered: boolean); virtual;
      procedure   DocBreak; virtual;
      procedure   DocImage; virtual;
      procedure   DocBold(Entered: boolean); virtual;
      procedure   DocCite(Entered: boolean); virtual;
      procedure   DocCode(Entered: boolean); virtual;
      procedure   DocEmphasized(Entered: boolean); virtual;
      procedure   DocItalic(Entered: boolean); virtual;
      procedure   DocKbd(Entered: boolean); virtual;
      procedure   DocPreformatted(Entered: boolean); virtual;
      procedure   DocSample(Entered: boolean); virtual;
      procedure   DocStrong(Entered: boolean); virtual;
      procedure   DocTeleType(Entered: boolean); virtual;
      procedure   DocVariable(Entered: boolean); virtual;
      procedure   DocList(Entered: boolean); virtual;
      procedure   DocOrderedList(Entered: boolean); virtual;
      procedure   DocListItem; virtual;
      procedure   DocDefList(Entered: boolean); virtual;
      procedure   DocDefTerm; virtual;
      procedure   DocDefExp; virtual;
      procedure   DocTable(Entered: boolean); virtual;
      procedure   DocTableRow(Entered: boolean); virtual;
      procedure   DocTableHeaderItem(Entered: boolean); virtual;
      procedure   DocTableItem(Entered: boolean); virtual;
      procedure   DocHorizontalRuler; virtual;
    end;

implementation

uses WUtils;

function TTextFile.GetLine(Idx: sw_integer; var S: string): boolean;
begin
  Abstract;
  GetLine:=false;
end;

constructor TMemoryTextFile.Init;
begin
  inherited Init;
  New(Lines, Init(500,500));
end;

procedure TMemoryTextFile.AddLine(const S: string);
begin
  Lines^.Insert(NewStr(S));
end;

function TMemoryTextFile.GetLine(Idx: sw_integer; var S: string): boolean;
var OK: boolean;
    PS: PString;
begin
  OK:=(Lines<>nil) and (Idx<Lines^.Count);
  if OK then
    begin
      PS:=Lines^.At(Idx);
      if PS=nil then S:='' else S:=PS^;
    end;
  GetLine:=OK;
end;

destructor TMemoryTextFile.Done;
begin
  inherited Done;
  if Lines<>nil then Dispose(Lines, Done); Lines:=nil;
end;

constructor TDOSTextFile.Init(AFileName: string);
(*{$ifdef TPUNIXLF}
  procedure readln(var t:text;var s:string);
  var
    c : char;
    i : longint;
  begin
    c:=#0;
    i:=0;
    while (not eof(t)) and (c<>#10) and (i<255) do
     begin
       read(t,c);
       if (i<255) and (c<>#10) then
   begin
     inc(i);
     s[i]:=c;
   end;
     end;
    if (i>0) and (s[i]=#13) then
       dec(i);
    s[0]:=chr(i);
   end;
{$endif}*)
var f: text;
    S: string;
begin
  inherited Init;
  if AFileName='' then Fail;
{$I-}
  Assign(f,AFileName);
  Reset(f);
  if IOResult<>0 then Fail;
  New(Lines, Init(500,2000));
  while (Eof(f)=false) and (IOResult=0) do
    begin
      readln(f,S); { this is the one in WUTILS.PAS }
      AddLine(S);
    end;
  Close(f);
{$I+}
end;

constructor TSGMLParser.Init;
begin
  inherited Init;
end;

function TSGMLParser.Process(HTMLFile: PTextFile): boolean;
var S: string;
    OK,LineOK: boolean;
begin
  if HTMLFile=nil then Exit;
  InTag:=false; InComment:=false; InString:=false; CurTag:='';
  Line:=0; OK:=true;
  repeat
    LineOK:=HTMLFile^.GetLine(Line,S);
    if LineOK then
      begin
        OK:=ProcessLine(S);
        Inc(Line);
      end;
  until (LineOK=false) or (OK=false);
  Process:=OK;
end;

function TSGMLParser.ProcessLine(LineText: string): boolean;
var OK: boolean;
    C: char;
    NewInString: boolean;
    OldInComment: boolean;
    WasThereAnyText: boolean;
    Pos2: integer;
    Name,Entity: string;
    LiteralCode: boolean;
    LiteralStart,LiteralEnd,P: integer;
const TabSize : integer = 8;
      Tab = #9;
begin
  WasThereAnyText:=false;
  OK:=true; LinePos:=1;
  LiteralStart:=0; LiteralEnd:=0;
  repeat
    P:=Pos(TAB,LineText);
    if P>0 then
      LineText:=copy(LineText,1,P-1)+CharStr(' ',TabSize)+copy(LineText,P+1,255);
  until P=0;
  while (LinePos<=length(LineText)) and OK do
    begin
      LiteralCode:=false;
      NewInString:=InString; OldInComment:=InComment;
      C:=LineText[LinePos];

      LiteralCode:=(LiteralStart<=LinePos) and (LinePos<=LiteralEnd);

      if (LiteralCode=false) and (C='&') then
        begin
          LiteralStart:=0; LiteralEnd:=0;
          Name:=''; Pos2:=LinePos+1;
          while (Pos2<=length(LineText)) and (LineText[Pos2]<>';') do
            begin
              Name:=Name+LineText[Pos2];
              Inc(Pos2);
            end;
          Inc(Pos2);
          if DocDecodeNamedEntity(Name,Entity) then
            begin
              LineText:=copy(LineText,1,LinePos-1)+Entity+copy(LineText,Pos2,255);
              LiteralStart:=LinePos; LiteralEnd:=LiteralStart+length(Entity)-1;
              C:=LineText[LinePos];
            end;
        end;

      LiteralCode:=(LiteralStart<=LinePos) and (LinePos<=LiteralEnd);

      if (LiteralCode=false) and (C='"') and (InTag=true) and (InString=false) then
        NewInString:=true;
      if (LiteralCode=false) and (C='<') and (InTag=false) then
        InTag:=true;

      if InTag then CurTag:=CurTag+C else
        WasThereAnyText:=DocAddTextChar(C);
      if (LiteralCode=false) and InTag and (InString=false) and (CurTag='<!--') then
        InComment:=true;
      { A comment can be longer than 255 chars
        move the test to LineText string,
        This is why the Previous, Next and Up Tags where not working ... PM
      if (LiteralCode=false) and InTag and InComment and (InString=false) and (length(CurTag)>=3) and
         (copy(CurTag,length(CurTag)-2,3)='-->') then
           InComment:=false; }
      if (LiteralCode=false) and InTag and InComment and (InString=false) and (LinePos>=3) and
         (copy(LineText,LinePos-2,3)='-->') then
           InComment:=false;

      if (LiteralCode=false) and (C='"') and (InTag=true) and (InString=true) then
        NewInString:=false;
      if (LiteralCode=false) and (C='>') and (InTag=true) then
        begin
          InTag:=false;
          if OldInComment then
            DocProcessComment(CurTag)
          else
            DocProcessTag(CurTag);
          CurTag:='';
        end;

      InString:=NewInString;
      Inc(LinePos);

    end;
  { whtml does not depend on whelp,
    so I can not use hscLineBreak here. PM }
  if InTag and InString then
    CurTag:=CurTag+#0
  else if WasThereAnyText then DocSoftBreak;

  ProcessLine:=true;
end;

procedure TSGMLParser.DocSoftBreak;
begin
  Abstract;
end;

function TSGMLParser.DocAddTextChar(C: char): boolean;
begin
  Abstract;
  DocAddTextChar:=false;
end;

procedure TSGMLParser.DocAddText(S: string);
var I: sw_integer;
begin
  for I:=1 to length(S) do
    DocAddTextChar(S[I]);
end;

function TSGMLParser.DocDecodeNamedEntity(Name: string; var Entity: string): boolean;
begin
  DocDecodeNamedEntity:=false;
end;

procedure TSGMLParser.DocProcessTag(Tag: string);
begin
  Abstract;
end;

procedure TSGMLParser.DocProcessComment(Comment: string);
begin
  Abstract;
end;

destructor TSGMLParser.Done;
begin
  inherited Done;
end;

procedure THTMLParser.DocSoftBreak;
begin
end;

function THTMLParser.DocAddTextChar(C: char): boolean;
begin
  { Abstract }
  DocAddTextChar:=false;
end;

function THTMLParser.DocDecodeNamedEntity(Name: string; var E: string): boolean;
var Found: boolean;
    Code: integer;
    CC: integer;
begin
  Found:=true; Code:=-1;
  Name:=LowCaseStr(Name);
  if copy(Name,1,1)='#' then
    begin
      Val(copy(Name,2,255),Code,CC);
      if CC<>0 then Code:=-1;
    end;
  if               (Name='lt')     then E:='<'   else { less-than sign                }
  if               (Name='gt')     then E:='>'   else { greater-than sign              }
  if               (Name='amp')    then E:='&'   else { ampersand                     }
  if               (Name='quot')   then E:='"'   else { double quote sign             }
  if (Code=160) or (Name='nbsp')   then E:=#255  else { no-break space                }
  if (Code=161) or (Name='iexcl')  then E:='≠'   else { inverted excalamation mark    }
  if (Code=162) or (Name='cent')   then E:='õ'   else { cent sign                     }
  if (Code=163) or (Name='pound')  then E:='ú'   else { pound sterling sign           }
  if (Code=164) or (Name='curren') then E:='$'   else { general currency sign         }
  if (Code=165) or (Name='yen')    then E:='ù'   else { yen sign                      }
  if (Code=166) or (Name='brvbar') then E:='|'   else { broken vertical bar           }
(*  if (Code=167) or (Name='sect')   then E:=#255  else { section sign                  }*)
(*  if (Code=168) or (Name='uml')    then E:=#255  else { umlaut  (dieresis)            }*)
  if (Code=169) or (Name='copy')   then E:='(C)' else { copyright sign                }
(*  if (Code=170) or (Name='ordf')   then E:=#255  else { ordinal indicator, feminine   }*)
  if (Code=171) or (Name='laquo')  then E:='"'   else { angle quotation mark -left    }
  if (Code=172) or (Name='not')    then E:='!'   else { not sign                      }
  if (Code=173) or (Name='shy')    then E:='-'   else { soft hypen                    }
  if (Code=174) or (Name='reg')    then E:='(R)' else { registered sign               }
(*  if (Code=175) or (Name='macr')   then E:='?'   else { macron                        }*)
  if (Code=176) or (Name='deg')    then E:='¯'   else { degree sign                   }
  if (Code=177) or (Name='plusmn') then E:='Ò'   else { plus-or-minus sign            }
  if (Code=178) or (Name='sup2')   then E:='˝'   else { superscript 2                 }
  if (Code=179) or (Name='sup3')   then E:='^3'  else { superscript 3                 }
  if (Code=180) or (Name='acute')  then E:=''''  else { acute accent                  }
  if (Code=181) or (Name='micro')  then E:='Ê'   else { micro sign                    }
(*  if (Code=182) or (Name='para')   then E:='?'   else { paragraph sign                }*)
  if (Code=183) or (Name='middot') then E:='˘'   else { middle dot                    }
(*  if (Code=184) or (Name='cedil')  then E:='?'   else { cedilla                       }*)
  if (Code=185) or (Name='sup1')   then E:='^1'  else { superscript 1                 }
(*  if (Code=186) or (Name='ordm')   then E:='?'   else { ordinal indicator, masculine  }*)
  if (Code=187) or (Name='raquo')  then E:='"'   else { angle quoatation mark -right  }
  if (Code=188) or (Name='frac14') then E:='¨'   else { fraction one-quarter          }
  if (Code=189) or (Name='frac12') then E:='´'   else { fraction one-half             }
  if (Code=190) or (Name='frac34') then E:='3/4' else { fraction three-quarters       }
  if (Code=191) or (Name='iquest') then E:='®'   else { inverted question mark        }
  if (Code=192) or (Name='Agrave') then E:='A'   else { capital A, grave accent       }
  if (Code=193) or (Name='Aacute') then E:='A'   else { capital A, acute accent       }
  if (Code=194) or (Name='Acirc')  then E:='A'   else { capital A, circumflex accent  }
  if (Code=195) or (Name='Atilde') then E:='A'   else { capital A, tilde accent       }
  if (Code=196) or (Name='Auml')   then E:='é'   else { capital A, dieresis or umlaut }
  if (Code=197) or (Name='Aring')  then E:='è'   else { capital A, ring               }
  if (Code=198) or (Name='AElig')  then E:='AE'  else { capital AE diphthong          }
(*  if (Code=199) or (Name='Ccedil') then E:='?'   else { capital C, cedilla            }*)
  if (Code=200) or (Name='Egrave') then E:='ê'   else { capital E, grave accent       }
  if (Code=201) or (Name='Eacute') then E:='ê'   else { capital E, acute accent       }
  if (Code=202) or (Name='Ecirc')  then E:='E'   else { capital E, circumflex accent  }
  if (Code=203) or (Name='Euml')   then E:='E'   else { capital E, dieresis or umlaut }
  if (Code=204) or (Name='Igrave') then E:='I'   else { capital I, grave accent       }
  if (Code=205) or (Name='Iacute') then E:='I'   else { capital I, acute accent       }
  if (Code=206) or (Name='Icirc')  then E:='I'   else { capital I, circumflex accent  }
  if (Code=207) or (Name='Iuml')   then E:='I'   else { capital I, dieresis or umlaut }
(*  if (Code=208) or (Name='ETH')    then E:='?'   else { capital Eth, Icelandic        }*)
  if (Code=209) or (Name='Ntidle') then E:='•'   else { capital N, tilde              }
  if (Code=210) or (Name='Ograve') then E:='O'   else { capital O, grave accent       }
  if (Code=211) or (Name='Oacute') then E:='O'   else { capital O, acute accent       }
  if (Code=212) or (Name='Ocirc')  then E:='O'   else { capital O, circumflex accent  }
  if (Code=213) or (Name='Otilde') then E:='O'   else { capital O, tilde              }
  if (Code=214) or (Name='Ouml')   then E:='ô'   else { capital O, dieresis or umlaut }
  if (Code=215) or (Name='times')  then E:='*'   else { multiply sign                 }
  if (Code=216) or (Name='Oslash') then E:='O'   else { capital O, slash              }
  if (Code=217) or (Name='Ugrave') then E:='U'   else { capital U, grave accent       }
  if (Code=218) or (Name='Uacute') then E:='U'   else { capital U, acute accent       }
  if (Code=219) or (Name='Ucirc')  then E:='U'   else { capital U, circumflex accent  }
  if (Code=220) or (Name='Uuml')   then E:='ö'   else { capital U, dieresis or umlaut }
  if (Code=221) or (Name='Yacute') then E:='Y'   else { capital Y, acute accent       }
(*  if (Code=222) or (Name='THORN')  then E:='?'   else { capital THORN, Icelandic      }*)
  if (Code=223) or (Name='szlig')  then E:='·'   else { small sharp S, German         }
  if (Code=224) or (Name='agrave') then E:='Ö'   else { small a, grave accent         }
  if (Code=225) or (Name='aacute') then E:='†'   else { small a, acute accent         }
  if (Code=226) or (Name='acirc')  then E:='É'   else { small a, circumflex accent    }
  if (Code=227) or (Name='atilde') then E:='É'   else { small a, tilde                }
  if (Code=228) or (Name='auml')   then E:='Ñ'   else { small a, dieresis or umlaut   }
  if (Code=229) or (Name='aring')  then E:='Ü'   else { small a, ring                 }
  if (Code=230) or (Name='aelig')  then E:='ae'  else { small ae, diphthong           }
(*  if (Code=231) or (Name='ccedil') then E:='?'   else { small c, cedilla              }*)
  if (Code=232) or (Name='egrave') then E:='ä'   else { small e, grave accent         }
  if (Code=233) or (Name='eacute') then E:='Ç'   else { small e, acute accent         }
  if (Code=234) or (Name='ecirc')  then E:='à'   else { small e, circumflex accent    }
  if (Code=235) or (Name='euml')   then E:='â'   else { small e, dieresis or umlaut   }
  if (Code=236) or (Name='igrave') then E:='ç'   else { small i, grave accent         }
  if (Code=237) or (Name='iacute') then E:='°'   else { small i, acute accent         }
  if (Code=238) or (Name='icirc')  then E:='å'   else { small i, circumflex accent    }
  if (Code=239) or (Name='iuml')   then E:='ã'   else { small i, dieresis or umlaut   }
(*  if (Code=240) or (Name='eth')    then E:='?'   else { small eth, Icelandic          }*)
  if (Code=241) or (Name='ntilde') then E:='§'   else { small n, tilde                }
  if (Code=242) or (Name='ograve') then E:='ï'   else { small o, grave accent         }
  if (Code=243) or (Name='oacute') then E:='¢'   else { small o, acute accent         }
  if (Code=244) or (Name='ocirc')  then E:='ì'   else { small o, circumflex accent    }
  if (Code=245) or (Name='otilde') then E:='ì'   else { small o, tilde                }
  if (Code=246) or (Name='ouml')   then E:='î'   else { small o, dieresis or umlaut   }
  if (Code=247) or (Name='divide') then E:='/'   else { divide sign                   }
  if (Code=248) or (Name='oslash') then E:='"'   else { small o, slash                }
  if (Code=249) or (Name='ugrave') then E:='ó'   else { small u, grave accent         }
  if (Code=250) or (Name='uacute') then E:='£'   else { small u, acute accent         }
  if (Code=251) or (Name='ucirc')  then E:='ñ'   else { small u, circumflex accent    }
  if (Code=252) or (Name='uuml')   then E:='Å'   else { small u, dieresis or umlaut   }
  if (Code=253) or (Name='yacute') then E:='y'   else { small y, acute accent         }
(*  if (Code=254) or (Name='thorn')  then E:='?'   else { small thorn, Icelandic        }*)
  if (Code=255) or (Name='yuml')   then E:='y'   else { small y, dieresis or umlaut   }
  if (Code=8217) then E:=''''   else                  { acute accent as generated by TeXH   }
  Found:=false;
  DocDecodeNamedEntity:=Found;
end;

procedure THTMLParser.DocProcessTag(Tag: string);
var UTagName,ETagName: string[30];
    P: byte;
    NotEndTag: boolean;
begin
  if copy(Tag,1,1)='<' then Delete(Tag,1,1);
  if copy(Tag,length(Tag),1)='>' then Delete(Tag,length(Tag),1);
  Tag:=Trim(Tag);
  P:=Pos(' ',Tag); if P=0 then P:=length(Tag)+1;
  TagName:=copy(Tag,1,P-1); TagParams:=copy(Tag,P+1,255);
  UTagName:=UpcaseStr(TagName);
  NotEndTag:=copy(TagName,1,1)<>'/';
  if NotEndTag then ETagName:=UTagName else ETagName:=copy(UTagName,2,255);

  if (UTagName='!DOCTYPE') then DocTYPE else
  { Section tags }
  if (ETagName='HTML') then DocHTML(NotEndTag) else
  if (ETagName='HEAD') then DocHEAD(NotEndTag) else
  if (ETagName='TITLE') then DocTITLE(NotEndTag) else
  if (ETagName='BODY') then DocBODY(NotEndTag) else
  { Anchor tags }
  if (ETagName='A') then DocAnchor(NotEndTag) else
  { Direct formatting directives }
  if (ETagName='H1') then DocHeading(1,NotEndTag) else
  if (ETagName='H2') then DocHeading(2,NotEndTag) else
  if (ETagName='H3') then DocHeading(3,NotEndTag) else
  if (ETagName='H4') then DocHeading(4,NotEndTag) else
  if (ETagName='H5') then DocHeading(5,NotEndTag) else
  if (ETagName='H6') then DocHeading(6,NotEndTag) else
  if (ETagName='P') then DocParagraph(NotEndTag) else
  if (ETagName='BR') then DocBreak else
  if (ETagName='B') then DocBold(NotEndTag) else
  if (ETagName='CITE') then DocCite(NotEndTag) else
  if (ETagName='CODE') then DocCode(NotEndTag) else
  if (ETagName='EM') then DocEmphasized(NotEndTag) else
  if (ETagName='I') then DocItalic(NotEndTag) else
  if (ETagName='KBD') then DocKbd(NotEndTag) else
  if (ETagName='PRE') then DocPreformatted(NotEndTag) else
  if (ETagName='SAMP') then DocSample(NotEndTag) else
  if (ETagName='STRONG') then DocStrong(NotEndTag) else
  if (ETagName='TT') then DocTeleType(NotEndTag) else
  if (ETagName='VAR') then DocVariable(NotEndTag) else
  { Unordered & ordered lists }
  if (ETagName='UL') then DocList(NotEndTag) else
  if (ETagName='OL') then DocOrderedList(NotEndTag) else
  if (UTagName='LI') then DocListItem else
  { Definition list }
  if (ETagName='DL') then DocDefList(NotEndTag) else
  if (UTagName='DT') then DocDefTerm else
  if (UTagName='DD') then DocDefExp else
  { Table }
  if (ETagName='TABLE') then DocTable(NotEndTag) else
  if (ETagName='TR') then DocTableRow(NotEndTag) else
  if (ETagName='TH') then DocTableHeaderItem(NotEndTag) else
  if (ETagName='TD') then DocTableItem(NotEndTag) else
  { Misc. tags }
  if (UTagName='META') then DocMETA else
  if (UTagName='IMG') then DocImage else
  if (UTagName='HR') then DocHorizontalRuler else
  DocUnknownTag;
end;

function THTMLParser.DocGetTagParam(Name: string; var Value: string): boolean;
var Found: boolean;
    S: string;
    ParamName,ParamValue: string;
    InStr: boolean;
    I: sw_integer;
begin
  Found:=false; Name:=UpcaseStr(Name);
  S:=TagParams;
  repeat
    InStr:=false;
    ParamName:=''; ParamValue:='';
    S:=Trim(S); I:=1;
    while (I<=length(S)) and (S[I]<>'=') do
      begin
        ParamName:=ParamName+S[I];
        Inc(I);
      end;
    ParamName:=Trim(ParamName);
    if S[I]='=' then
    begin
      Inc(I); InStr:=false;
      while (I<=length(S)) and (S[I]=' ') do
        Inc(I);
      if (I<=length(S)) and (S[I]='"') then
        begin
          InStr:=true;
          Inc(I);
        end;
      while (I<=length(S)) and ((InStr=true) or (S[I]<>' ')) do
        begin
          if S[I]='"' then
            begin
              InStr:=not InStr;
              if InStr=false then Break;
            end
          else
            ParamValue:=ParamValue+S[I];
          Inc(I);
        end;
    end;
    Found:=(Name=UpcaseStr(ParamName));
    if Found then Value:=ParamValue;
    Delete(S,1,I);
  until Found or (S='');
  DocGetTagParam:=Found;
end;

procedure THTMLParser.DocProcessComment(Comment: string);
begin
end;

procedure THTMLParser.DocUnknownTag;
begin
end;

procedure THTMLParser.DocTYPE;
begin
end;

procedure THTMLParser.DocHTML(Entered: boolean);
begin
end;

procedure THTMLParser.DocHEAD(Entered: boolean);
begin
end;

procedure THTMLParser.DocMETA;
begin
end;

procedure THTMLParser.DocTITLE(Entered: boolean);
begin
end;

procedure THTMLParser.DocBODY(Entered: boolean);
begin
end;

procedure THTMLParser.DocAnchor(Entered: boolean);
begin
end;

procedure THTMLParser.DocHeading(Level: integer; Entered: boolean);
begin
end;

procedure THTMLParser.DocParagraph(Entered: boolean);
begin
end;

procedure THTMLParser.DocBreak;
begin
end;

procedure THTMLParser.DocImage;
begin
end;

procedure THTMLParser.DocBold(Entered: boolean);
begin
end;

procedure THTMLParser.DocCite(Entered: boolean);
begin
end;

procedure THTMLParser.DocCode(Entered: boolean);
begin
end;

procedure THTMLParser.DocEmphasized(Entered: boolean);
begin
end;

procedure THTMLParser.DocItalic(Entered: boolean);
begin
end;

procedure THTMLParser.DocKbd(Entered: boolean);
begin
end;

procedure THTMLParser.DocPreformatted(Entered: boolean);
begin
end;

procedure THTMLParser.DocSample(Entered: boolean);
begin
end;

procedure THTMLParser.DocStrong(Entered: boolean);
begin
end;

procedure THTMLParser.DocTeleType(Entered: boolean);
begin
end;

procedure THTMLParser.DocVariable(Entered: boolean);
begin
end;

procedure THTMLParser.DocList(Entered: boolean);
begin
end;

procedure THTMLParser.DocOrderedList(Entered: boolean);
begin
end;

procedure THTMLParser.DocListItem;
begin
end;

procedure THTMLParser.DocDefList(Entered: boolean);
begin
end;

procedure THTMLParser.DocDefTerm;
begin
end;

procedure THTMLParser.DocDefExp;
begin
end;

procedure THTMLParser.DocTable(Entered: boolean);
begin
end;

procedure THTMLParser.DocTableRow(Entered: boolean);
begin
end;

procedure THTMLParser.DocTableHeaderItem(Entered: boolean);
begin
end;

procedure THTMLParser.DocTableItem(Entered: boolean);
begin
end;

procedure THTMLParser.DocHorizontalRuler;
begin
end;



END.
