unit WideStrUtils;

{$mode objfpc}
{$H+}
{$inline on}

interface

uses
  SysUtils;

function WideStringReplace(const S, OldPattern, NewPattern: WideString; Flags: TReplaceFlags): WideString;
function WideReplaceStr(const AText, AFromText, AToText: WideString): WideString; inline;
function WideReplaceText(const AText, AFromText, AToText: WideString): WideString; inline;

function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString; Flags: TReplaceFlags): UnicodeString;
function UnicodeReplaceStr(const AText, AFromText, AToText: UnicodeString): UnicodeString; inline;
function UnicodeReplaceText(const AText, AFromText, AToText: UnicodeString): UnicodeString; inline;


implementation

function WideReplaceStr(const AText, AFromText, AToText: WideString): WideString; inline;
begin
  Result := WideStringReplace(AText, AFromText, AToText, [rfReplaceAll]);
end;

function WideReplaceText(const AText, AFromText, AToText: WideString): WideString; inline;
begin
  Result := WideStringReplace(AText, AFromText, AToText, [rfReplaceAll, rfIgnoreCase]);
end;

function UnicodeReplaceStr(const AText, AFromText, AToText: UnicodeString): UnicodeString; inline;
begin
  Result := UnicodeStringReplace(AText, AFromText, AToText, [rfReplaceAll]);
end;

function UnicodeReplaceText(const AText, AFromText, AToText: UnicodeString): UnicodeString; inline;
begin
  Result := UnicodeStringReplace(AText, AFromText, AToText, [rfReplaceAll, rfIgnoreCase]);
end;

Function WideStringReplace(const S, OldPattern, NewPattern: WideString; Flags: TReplaceFlags): WideString;

begin
  Result:= sysutils.WideStringReplace(S,OldPattern,NewPattern,Flags);
end;

Function UnicodeStringReplace(const S, OldPattern, NewPattern: UnicodeString; Flags: TReplaceFlags): UnicodeString;

begin
  Result:= sysutils.UnicodeStringReplace(S,OldPattern,NewPattern,Flags);
end;

end.

