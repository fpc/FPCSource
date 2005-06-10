program GetCountryInfo;

{$IFNDEF OS2}
 Sorry, this code is for OS/2 only...
{$ENDIF}

uses
{$IFDEF FPC}
 DosCalls;
{$ELSE}
 Os2Def,
 {$IFDEF SPEED}
 BseDos;
 {$ELSE}
 DosProcs, DosTypes;
 {$ENDIF}
{$ENDIF}

type
 cardinal = longint;

{$IFDEF FPC}
const
 NO_ERROR = 0;
{$ENDIF}

var
{$IFDEF VER70} (* patched Borland Pascal *)
 Country: TCountryCode;
 CtryInfo: TCountryInfo;
 Size: longint;
{$ELSE}
 Country: COUNTRYCODE;  (* Country code info (0 = current country) *)
 CtryInfo: COUNTRYINFO; (* Buffer for country-specific information *)
 Size: cardinal;        (* Real size of returned data              *)
{$ENDIF}
 W: word;

begin
 WriteLn;
 Size := 0;
 FillChar (Country, SizeOf (Country), 0);
 FillChar (CtryInfo, SizeOf (CtryInfo), 0);
 W :=
{$IFDEF VER70}
      DosGetCtryInfo
{$ELSE}
      DosQueryCtryInfo
{$ENDIF}
                       (SizeOf (CtryInfo), Country, CtryInfo, Size);
 if (W <> NO_ERROR) then
 begin
  WriteLn ('DosQueryCtryInfo error: return code = ', W);
  Halt (1);
 end;
 WriteLn ('Code of the country is ', CtryInfo.Country,
                                  ', current codepage is ', CtryInfo.CodePage);
end.
