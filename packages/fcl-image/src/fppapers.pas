{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2025 by the Free Pascal development team

    List of Paper Sizes in Inches and Cm.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ 2025 Massimo Magnano }

unit FpPapers;

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Types, FpImage, FpUnitOfMeasure;
{$ELSE FPC_DOTTEDUNITS}
uses
  Types, FpImage, FpUnitOfMeasure;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Description of a paper size }
  TPaperSize =
  {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
  packed
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
  record
    name: String[16];
    w, h: Single;
  end;

  TPaperSizes=array of TPaperSize;
  PPaperSizes=^TPaperSizes;

const
  Paper_A_cm: TPaperSizes=(
  (name:'A0'; w:84.1; h:118.9), (name:'A1'; w:59.4; h:84.1), (name:'A2'; w:42.0; h:59.4),
  (name:'A3'; w:29.7; h:42.0), (name:'A4'; w:21.0; h:29.7), (name:'A5'; w:14.8; h:21.0),
  (name:'A6'; w:10.5; h:14.8), (name:'A7'; w:7.4; h:10.5), (name:'A8'; w:5.2; h:7.4),
  (name:'A9'; w:3.7; h:5.2), (name:'A10'; w:2.6; h:3.7)
  );

  Paper_A_inch: TPaperSizes=(
  (name:'A0'; w:33.111; h:46.812), (name:'A1'; w:23.387; h:33.111), (name:'A2'; w:16.536; h:23.387),
  (name:'A3'; w:11.694; h:16.536), (name:'A4'; w:8.269; h:11.694), (name:'A5'; w:5.828; h:8.269),
  (name:'A6'; w:4.135; h:5.828), (name:'A7'; w:2.914; h:4.135), (name:'A8'; w:2.048; h:2.914),
  (name:'A9'; w:1.458; h:2.048), (name:'A10'; w:1.025; h:1.458)
  );

  Paper_B_cm: TPaperSizes=(
  (name:'B0'; w:100.0; h:141.4), (name:'B1'; w:70.7; h:100.0), (name:'B2'; w:50.0; h:70.7),
  (name:'B3'; w:35.3; h:50.0), (name:'B4'; w:25.0; h:35.3), (name:'B5'; w:17.6; h:25.0),
  (name:'B6'; w:12.5; h:17.6), (name:'B7'; w:8.8; h:12.5), (name:'B8'; w:6.2; h:8.8),
  (name:'B9'; w:4.4; h:6.2), (name:'B10'; w:3.1; h:4.4)
  );

  Paper_B_inch: TPaperSizes=(
  (name:'B0'; w:39.371; h:55.670), (name:'B1'; w:27.836; h:39.371), (name:'B2'; w:19.686; h:27.836),
  (name:'B3'; w:13.899; h:19.686), (name:'B4'; w:9.844; h:13.899), (name:'B5'; w:6.930; h:9.844),
  (name:'B6'; w:4.922; h:6.930), (name:'B7'; w:3.466; h:4.922), (name:'B8'; w:2.442; h:3.466),
  (name:'B9'; w:1.733; h:2.442), (name:'B10'; w:1.221; h:1.733)
  );

  Paper_C_cm: TPaperSizes=(
  (name:'C0'; w:91.7; h:129.7), (name:'C1'; w:64.8; h:91.7), (name:'C2'; w:45.8; h:64.8),
  (name:'C3'; w:32.4; h:45.8), (name:'C4'; w:22.9; h:32.4), (name:'C5'; w:16.2; h:22.9),
  (name:'C6'; w:11.4; h:16.2), (name:'C7'; w:8.1; h:11.4), (name:'C8'; w:5.7; h:8.1),
  (name:'C9'; w:4.0; h:5.7), (name:'C10'; w:2.8; h:4.0)
  );

  Paper_C_inch: TPaperSizes=(
  (name:'C0'; w:36.103; h:51.064), (name:'C1'; w:25.513; h:36.103), (name:'C2'; w:18.032; h:25.513),
  (name:'C3'; w:12.757; h:18.032), (name:'C4'; w:9.017; h:12.757), (name:'C5'; w:6.379; h:9.017),
  (name:'C6'; w:4.489; h:6.379), (name:'C7'; w:3.190; h:4.489), (name:'C8'; w:2.245; h:3.190),
  (name:'C9'; w:1.576; h:2.245), (name:'C10'; w:1.103; h:1.576)
  );

  Paper_DIN_476_cm: TPaperSizes=((name:'2A0'; w:118.9; h:168.2), (name:'4A0'; w:168.2; h:237.8));
  Paper_DIN_476_inch: TPaperSizes=((name:'2A0'; w:46.812; h:66.221), (name:'4A0'; w:66.221; h:93.623));

  Paper_JIS_cm: TPaperSizes=(
  (name:'B0'; w:103.0; h:145.6), (name:'B1'; w:72.8; h:103.0), (name:'B2'; w:51.5; h:72.8),
  (name:'B3'; w:36.4; h:51.5), (name:'B4'; w:25.7; h:36.4), (name:'B5'; w:18.2; h:25.7),
  (name:'B6'; w:12.8; h:18.2), (name:'B7'; w:9.1; h:12.8), (name:'B8'; w:6.4; h:9.1),
  (name:'B9'; w:4.5; h:6.4), (name:'B10'; w:3.2; h:4.5), (name:'B11'; w:2.2; h:3.2),
  (name:'B12'; w:1.6; h:2.2)
  );

  Paper_JIS_inch: TPaperSizes=(
  (name:'B0'; w:40.552; h:57.324), (name:'B1'; w:28.662; h:40.552), (name:'B2'; w:20.277; h:28.662),
  (name:'B3'; w:14.332; h:20.277), (name:'B4'; w:10.119; h:14.332), (name:'B5'; w:7.166; h:10.119),
  (name:'B6'; w:5.040; h:7.166), (name:'B7'; w:3.584; h:5.040), (name:'B8'; w:2.521; h:3.584),
  (name:'B9'; w:1.773; h:2.521), (name:'B10'; w:1.261; h:1.773), (name:'B11'; w:0.867; h:1.261),
  (name:'B12'; w:0.631; h:0.867)
  );

  Paper_Shiroku_ban_cm: TPaperSizes=(
  (name:'B4'; w:26.4; h:37.9), (name:'B5'; w:18.9; h:26.2), (name:'B6'; w:12.7; h:18.8)
  );

  Paper_Shiroku_ban_inch: TPaperSizes=(
  (name:'B4'; w:10.395; h:14.922), (name:'B5'; w:7.442; h:10.316), (name:'B6'; w:5.001; h:7.403)
  );

  Paper_Kiku_cm: TPaperSizes=(
  (name:'B4'; w:22.7; h:30.6), (name:'B5'; w:15.1; h:22.7)
  );

  Paper_Kiku_inch: TPaperSizes=(
  (name:'B4'; w:8.938; h:12.048), (name:'B5'; w:5.946; h:8.938)
  );

  Paper_US_cm: TPaperSizes=(
  (name:'Half Letter'; w:21.6; h:14.0), (name:'Letter'; w:21.6; h:27.9), (name:'Government Legal'; w:21.6; h:33.0),
  (name:'Executive'; w:18.4; h:26.7), (name:'Statement'; w:14.0; h:21.6), (name:'Legal'; w:21.6; h:35.6),
  (name:'Ledger'; w:43.2; h:27.9), (name:'Tabloid'; w:27.9; h:43.2), (name:'Junior Legal'; w:20.3; h:12.7)
  );

  Paper_US_inch: TPaperSizes=(
  (name:'Half Letter'; w:8.5; h:5.5), (name:'Letter'; w:8.5; h:11.0), (name:'Government Legal'; w:8.5; h:13.0),
  (name:'Executive'; w:7.25; h:10.5), (name:'Statement'; w:5.5; h:8.5), (name:'Legal'; w:8.5; h:14.0),
  (name:'Ledger'; w:17.0; h:11.0), (name:'Tabloid'; w:11.0; h:17.0), (name:'Junior Legal'; w:8.0; h:5.0)
  );

  Paper_ANSI_cm: TPaperSizes=(
  (name:'A'; w:21.6; h:27.9), (name:'B'; w:43.2; h:27.9), (name:'C'; w:43.2; h:55.9),
  (name:'D'; w:55.9; h:86.4), (name:'E'; w:86.4; h:111.8)
  );

  Paper_ANSI_inch: TPaperSizes=(
  (name:'A'; w:8.5; h:11.0), (name:'B'; w:17.0; h:11.0), (name:'C'; w:17.0; h:22.0),
  (name:'D'; w:22.0; h:34.0), (name:'E'; w:34.0; h:44.0)
  );

  //Given the variety of formats in various countries, we put together a mix of the most common ones
  Photo_cm: TPaperSizes=(
  (name:'EU Pass'; w:3.0; h:4.0),
  (name:'Card ID'; w:3.5; h:4.5),
  (name:''; w:7; h:10),
  (name:''; w:9; h:12), (name:''; w:9; h:13),
  (name:''; w:10; h:10), (name:''; w:10; h:13), (name:''; w:10; h:15),
  (name:''; w:12; h:18),
  (name:''; w:13; h:13), (name:''; w:13; h:17), (name:''; w:13; h:18), (name:''; w:13; h:19),
  (name:''; w:15; h:15), (name:''; w:15; h:20), (name:''; w:15; h:21),
  (name:''; w:20; h:20), (name:''; w:20; h:24), (name:''; w:20; h:25), (name:''; w:20; h:30),
  (name:''; w:30; h:30), (name:''; w:30; h:40), (name:''; w:30; h:45),
  (name:''; w:40; h:40), (name:''; w:40; h:50), (name:''; w:40; h:60)
  );

  Photo_inch: TPaperSizes=(
  (name:'EU Pass'; w:1.182; h:1.575),
  (name:'Card ID'; w:1.378; h:1.772),
  (name:''; w:2.756; h:3.938),
  (name:''; w:3.544; h:4.725), (name:''; w:3.544; h:5.119),
  (name:''; w:3.938; h:3.938), (name:''; w:3.938; h:5.119), (name:''; w:3.938; h:5.91),
  (name:''; w:4.725; h:7.087),
  (name:''; w:5.119; h:5.119), (name:''; w:5.119; h:6.693), (name:''; w:5.119; h:7.087), (name:''; w:5.119; h:7.481),
  (name:''; w:5.91; h:5.91), (name:''; w:5.91; h:7.875), (name:''; w:5.91; h:8.268),
  (name:''; w:7.875; h:7.875), (name:''; w:7.875; h:9.449), (name:''; w:7.875; h:9.843), (name:''; w:7.875; h:11.812),
  (name:''; w:11.812; h:11.812), (name:''; w:11.812; h:15.749), (name:''; w:11.812; h:17.717),
  (name:''; w:15.749; h:15.749), (name:''; w:15.749; h:19.686), (name:''; w:15.749; h:23.623)
  );

  Paper_BUSINESS_CARD_cm: TPaperSizes=(
  (name:'A8'; w:7.4; h:5.2), (name:'B8'; w:8.8; h:6.2), (name:'West Europe'; w:8.5; h:5.5),
  (name:'International'; w:8.6; h:5.4),
  (name:'North America'; w:8.9; h:5.1),
  (name:'East Europe,Asia'; w:9.0; h:5.0),
  (name:'East Asia'; w:9.0; h:5.4), (name:'Oceania'; w:9.0; h:5.5), (name:'Japan'; w:9.1; h:5.5)
  );

  Paper_BUSINESS_CARD_inch: TPaperSizes=(
  (name:'A8'; w:2.914; h:2.048), (name:'B8'; w:3.466; h:2.442), (name:'West Europe'; w:3.347; h:2.166),
  (name:'International'; w:3.387; h:2.127),
  (name:'North America'; w:3.5; h:2.0),
  (name:'East Europe,Asia'; w:3.544; h:1.970),
  (name:'East Asia'; w:3.544; h:2.127), (name:'Oceania'; w:3.544; h:2.166), (name:'Japan'; w:3.584; h:2.166)
  );

{** Convert a Paper Array To/From Cm/Inches dimensions }
function Sizes_InchToCm(const APapers:TPaperSizes):TPaperSizes;
function Sizes_CmToInch(const APapers:TPaperSizes):TPaperSizes;

{** Returns the smallest Paper in PaperSizes array that can contain the specified dimensions }
function GetPaperSize(AWidth, AHeight:Single; PaperSizes:array of TPaperSizes): TPaperSize;

implementation

function Sizes_InchToCm(const APapers: TPaperSizes): TPaperSizes;
var
   i:Integer;

begin
  Result :=Copy(APapers, 0, Length(APapers));
  for i:=Low(Result) to High(Result) do
  begin
    Result[i].w :=Result[i].w*2.54;
    Result[i].h :=Result[i].h*2.54;
  end;
end;

function Sizes_CmToInch(const APapers: TPaperSizes): TPaperSizes;
var
   i:Integer;

begin
  Result :=Copy(APapers, 0, Length(APapers));
  for i:=Low(Result) to High(Result) do
  begin
    Result[i].w :=Result[i].w/2.54;
    Result[i].h :=Result[i].h/2.54;
  end;
end;

function GetPaperSize(AWidth, AHeight: Single; PaperSizes: array of TPaperSizes): TPaperSize;
var
   p, i: Integer;
   curW, curH: Single;

begin
  curW:= MAXINT; curH:= MAXINT;
  Fillchar(Result, sizeof(TPaperSize), 0);

  for p:=Low(PaperSizes) to High(PaperSizes) do
  for i:=Low(PaperSizes[p]) to High(PaperSizes[p]) do
  begin
    //Current paper can contain AWidth x AHeight ?
    if (PaperSizes[p][i].w >= AWidth) and (PaperSizes[p][i].h >= AHeight) then
    begin
      //Current paper is smallest then Result ?
      if (PaperSizes[p][i].w <= curW) and (PaperSizes[p][i].h <= curH) then
      begin
        Result:= PaperSizes[p][i];
        curW:= Result.w;
        curH:= Result.h;
      end;
    end;
  end;
end;

end.

