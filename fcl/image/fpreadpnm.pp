{*****************************************************************************}
{
    $Id$
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    PNM writer implementation.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
{The PNM (Portable aNyMaps) is a generic name for :
  PBM : Portable BitMaps,
  PGM : Portable GrayMaps,
  PPM : Portable PixMaps.
There is no file format associated  with PNM itself.}
{$mode objfpc}{$h+}
unit FPReadPNM;
interface

uses FPImage, classes, sysutils;

type
  TFPReaderPNM=class (TFPCustomImageReader)
    private
      BitMapType:Integer;
    protected
      function  InternalCheck (Stream:TStream):boolean;override;
      procedure InternalRead(Stream:TStream;Img:TFPCustomImage);
  end;

implementation

function TFPReaderPNM.InternalCheck(Stream:TStream):boolean;
  var
    StrBitMapType:String[3];
  begin
    InternalCheck:=False;
    with stream do
      StrBitMapType[0]:=Chr(Read(StrBitMapType[1],2));
    BitMapType:=Ord(StrBitMapType[2])-Ord('0');
    InternalCheck:=(Length(StrBitMapType)=2)and(StrBitMapType[1]='P')and(BitMapType in [1..6]);
end;
{TODO : real implementation of InternalRead}
procedure TFPReaderPNM.InternalRead(Stream:TStream;Img:TFPCustomImage);
  procedure ReadHeader;
    const
{Whitespace (TABs, CRs, LFs, blanks) are separators in the PNM Headers}
      WhiteSpaces=[#9,#10,#13,#32];
    function DropWhiteSpaces:Char;
      begin
        with Stream do
          repeat
            Read(DropWhiteSpaces,1);
          until not(DropWhiteSpaces in WhiteSpaces);
      end;
    function ReadInteger:Integer;
      var
        s:String[7];
      begin
        s:='';
        s[1]:=DropWhiteSpaces;
        with Stream do
          repeat
            Inc(s[0]);
            Read(s[Length(s)+1],1)
          until s[Length(s)+1] in WhiteSpaces;
        Val(s,ReadInteger);
      end;
    begin
      Img.SetSize(ReadInteger,ReadInteger);
      WriteLn(ReadInteger);
    end;
  var
    Row,Coulumn,nBpLine,ReadSize:Integer;
    aColor:TFPcolor;
    aLine:PByte;
  begin
    case BitMapType of
      1:nBpLine:=Img.Width*2;{p p p}
      2:nBpLine:=Img.Width*4;{lll lll lll}
      3:nBpLine:=Img.Width*3*4;{rrr ggg bbb rrr ggg bbb}
      4:begin
          nBpLine:=Img.Width SHR 3;
          if(Img.Width AND $0F)<>0
          then
            Inc(nBpLine);
        end;
      5:nBpLine:=Img.Width;
      6:nBpLine:=Img.Width*3;
    end;
    GetMem(aLine,nBpLine);
    for Row:=img.Height-1 downto 0 do
      begin
        Stream.Read(aLine^,nBpLine);
        for Coulumn:=0 to img.Width-1 do
          with aColor do
            begin
              case BitMapType of
                1:;
                2:;
                3:;
                4:;
                5:;
                6:;
              end;
              alpha:=AlphaOpaque;
              img.colors[Coulumn,Row]:=aColor;
            end;
      end;
        FreeMem(aLine,nBpLine);
  end;

initialization
  ImageHandlers.RegisterImageReader ('PNM Format', 'PNM', TFPReaderPNM);
end.
{
$Log$
Revision 1.1  2003-09-30 07:15:48  mazen
+ Support for PNM (Portable aNyMap) formats (skeleton only)
   need to complete implementation

}
