{
  Display width of Unicode text in terminal columns, used by the %t conversion of Format.
  This file is part of the Free Pascal run time library.
  See the file COPYING.FPC, included in this distribution, for details about the copyright.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit unicodedisplaywidth;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}
{$h+}

interface

var
  // Columns of a grapheme cluster whose East Asian width is Ambiguous: 1, or 2 for CJK text.
  AmbiguousDisplayWidth : Integer = 1;

// Columns of one extended grapheme cluster: 0 when empty, 2 when East Asian wide or fullwidth, else 1.
function ExtendedGraphemeClusterDisplayWidth(const aCluster: UnicodeString): Integer;
// Columns of aText, summing its extended grapheme clusters.
function StringDisplayWidth(const aText: UnicodeString): SizeInt;
// SysUtils.TStringDisplayWidthFunc measuring extended grapheme clusters; this unit installs it.
function GraphemeDisplayWidth(const aText: UnicodeString; aMaxWidth: SizeInt; out aWidth: SizeInt): SizeInt;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Unicode.Graphemebreakproperty, System.Unicode.Eastasianwidth;
{$ELSE FPC_DOTTEDUNITS}
  SysUtils, graphemebreakproperty, eastasianwidth;
{$ENDIF FPC_DOTTEDUNITS}

var
  OldStringDisplayWidthFunc : TStringDisplayWidthFunc;


function ExtendedGraphemeClusterDisplayWidth(const aCluster: UnicodeString): Integer;

var
  lFirst : UCS4Char;

begin
  if Length(aCluster)=0 then
    Exit(0);
  lFirst:=Ord(aCluster[1]);
  if (lFirst>=$D800) and (lFirst<=$DBFF) and (Length(aCluster)>=2)
     and (Ord(aCluster[2])>=$DC00) and (Ord(aCluster[2])<=$DFFF) then
    lFirst:=$10000+(((lFirst-$D800) shl 10) or (Ord(aCluster[2])-$DC00));
  case GetEastAsianWidth(lFirst) of
    eawW, eawF:
      Result:=2;
    eawA:
      Result:=AmbiguousDisplayWidth;
  else
    Result:=1;
  end;
end;


function StringDisplayWidth(const aText: UnicodeString): SizeInt;

begin
  GraphemeDisplayWidth(aText,-1,Result);
end;


function GraphemeDisplayWidth(const aText: UnicodeString; aMaxWidth: SizeInt; out aWidth: SizeInt): SizeInt;

var
  lClusters : TUnicodeStringExtendedGraphemeClustersEnumerator;
  lCluster : UnicodeString;
  lColumns : Integer;

begin
  Result:=0;
  aWidth:=0;
  lClusters:=TUnicodeStringExtendedGraphemeClustersEnumerator.Create(aText);
  try
    while lClusters.MoveNext do
      begin
      lCluster:=lClusters.Current;
      lColumns:=ExtendedGraphemeClusterDisplayWidth(lCluster);
      if (aMaxWidth>=0) and (aWidth+lColumns>aMaxWidth) then
        Break;
      Inc(Result,Length(lCluster));
      Inc(aWidth,lColumns);
      end;
  finally
    lClusters.Free;
  end;
end;


initialization
  OldStringDisplayWidthFunc:=StringDisplayWidthFunc;
  StringDisplayWidthFunc:=@GraphemeDisplayWidth;
finalization
  if StringDisplayWidthFunc=@GraphemeDisplayWidth then
    StringDisplayWidthFunc:=OldStringDisplayWidthFunc;
end.
