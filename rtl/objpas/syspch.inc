{
    *********************************************************************
    $Id$
    Copyright (C) 1997, 1998 Gertjan Schouten

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    *********************************************************************

    System Utilities For Free Pascal
}

{  PChar functions  }

type
   pbyte = ^byte;
   CharArray = array[0..0] of char;

{  StrLen returns the length of Str ( terminator not included )  }

function StrLen(Str: PChar): cardinal;
begin
result := 0;
if Str <> nil then begin
   while CharArray(Str^)[result] <> #0 do
      result := result + 1;
   end ;
end ;

{  StrEnd returns a pointer to the last character (terminator) of Str  }

function StrEnd(Str: PChar): PChar;
begin
result := Str;
if Str <> nil then begin
   while result^ <> #0 do
      result := result + 1;
   end ;
end ;

{  StrMove copies Count bytes from source to dest, source and dest may overlap.   }

function StrMove(Dest, Source: PChar; Count: cardinal): PChar;
begin
result := Dest;
if (Dest <> nil) and (Source <> nil) and (Count > 0) then
   move(Source^, Dest^, Count);
end ;

{  StrCopy copies StrLen(Source) characters from Source to Dest and returns Dest  }

function StrCopy(Dest, Source: PChar): PChar;
begin
result := StrMove(Dest, Source, 1 + StrLen(Source));  {  copy nul character too !  }
end ;

{  StrECopy copies StrLen(Source) characters from Source to Dest and returns StrEnd(Dest)  }

function StrECopy(Dest, Source: PChar): PChar;
begin
StrMove(Dest, Source, 1 + StrLen(Source));
result := StrEnd(Dest);
end ;

{  StrLCopy copies MaxLen or less characters from Source to Dest and returns Dest  }

function StrLCopy(Dest, Source: PChar; MaxLen: cardinal): PChar;
var count: cardinal;
begin
result := Dest;
if result <> Nil then begin
   count := StrLen(Source);
   if count > MaxLen then
      count := MaxLen;
   StrMove(Dest, Source, count);
   CharArray(result^)[Count] := #0; { terminate ! }
   end ;
end ;

{  StrPCopy copies the pascal string Source to Dest and returns Dest  }

function StrPCopy(Dest: PChar; Source: string): PChar;
begin
result := StrMove(Dest, PChar(@Source[1]), length(Source));
end ;

{  StrPLCopy copies MaxLen or less characters from the pascal string
   Source to Dest and returns Dest  }

function StrPLCopy(Dest: PChar; Source: string; MaxLen: cardinal): PChar;
var Count: cardinal;
begin
result := Dest;
if (Result <> Nil) and (MaxLen <> 0) then begin
   Count := Length(Source);
   if Count > MaxLen then
      Count := MaxLen;
   StrMove(Dest, PChar(@Source[1]), Count);
   CharArray(result^)[Count] := #0;  { terminate ! }
   end ;
end ;

{  StrCat concatenates Dest and Source and returns Dest  }

function StrCat(Dest, Source: PChar): PChar;
begin
result := Dest;
StrMove(StrEnd(Dest), Source, 1 + StrLen(Source)); {  include #0  }
end ;

{  StrLCat concatenates Dest and MaxLen - StrLen(Dest) (or less) characters
   from Source, and returns Dest   }

function StrLCat(Dest, Source: PChar; MaxLen: cardinal): PChar;
var Count: cardinal; P: PChar;
begin
result := Dest;
if (Dest <> nil) and (MaxLen <> 0) then begin
   P := StrEnd(Dest);
   Count := StrLen(Source);
   if Count > MaxLen - (P - Dest) then
      Count := MaxLen - (P - Dest);
   if Count <> 0 then begin
      StrMove(P, Source, Count);
      CharArray(p^)[Count] := #0;       {  terminate Dest  }
      end ;
   end ;
end ;

{  StrComp returns 0 if Str1 and Str2 are equal,
   a value less than 0 in case Str1 < Str2
   and a value greater than 0 in case Str1 > Str2  }

function StrComp(Str1, Str2: PChar): integer;
begin
result := 0;
if (Str1 <> Nil) and (Str2 <> Nil) then begin
   while result = 0 do begin
      result := byte(Str1^) - byte(Str2^);
      if (Str1^ = #0) or (Str2^ = #0) then break;
      Str1 := Str1 + 1;
      Str2 := Str2 + 1;
      end ;
   end ;
end ;

{  StrIComp returns 0 if Str1 and Str2 are equal,
   a value less than 0 in case Str1 < Str2
   and a value greater than 0 in case Str1 > Str2;
   comparison is case insensitive  }

function StrIComp(Str1, Str2: PChar): integer;
var Chr1, Chr2: byte;
begin
result := 0;
if (Str1 <> Nil) and (Str2 <> Nil) then begin
   while result = 0 do begin
      Chr1 := byte(Str1^);
      Chr2 := byte(Str2^);
      if Chr1 in [97..122] then Chr1 := Chr1 - 32;
      if Chr2 in [97..122] then Chr2 := Chr2 - 32;
      result := Chr1 - Chr2;
      if (Chr1 = 0) or (Chr2 = 0) then break;
      Str1 := Str1 + 1;
      Str2 := Str2 + 1;
      end ;
   end ;
end ;

{  StrLComp returns 0 if Str1 and Str2 are equal,
   a value less than 0 in case Str1 < Str2
   and a value greater than 0 in case Str1 > Str2;
   MaxLen or less characters are compared  }

function StrLComp(Str1, Str2: PChar; MaxLen: cardinal): integer;
var I: integer;
begin
result := 0;
if (Str1 <> Nil) and (Str2 <> Nil) then begin
   I := 0;
   while (I < MaxLen) and (result = 0) do begin
      result := byte(Str1^) - byte(Str2^);
      if (Str1^ = #0) or (Str2^ = #0) then break;
      Str1 := Str1 + 1;
      Str2 := Str2 + 1;
      I := I + 1;
      end ;
   end ;
end ;

{  StrLIComp returns 0 if Str1 and Str2 are equal,
   a value less than 0 in case Str1 < Str2
   and a value greater than 0 in case Str1 > Str2;
   comparison is case insensitive and MaxLen or less characters are compared  }

function StrLIComp(Str1, Str2: PChar; MaxLen: cardinal): integer;
var Chr1, Chr2: byte; I: integer;
begin
result := 0;
if (Str1 <> Nil) and (Str2 <> Nil) then begin
   I := 0;
   while (I < MaxLen) and (result = 0) do begin
      Chr1 := byte(Str1^);
      Chr2 := byte(Str2^);
      if Chr1 in [97..122] then Chr1 := Chr1 - 32;
      if Chr2 in [97..122] then Chr2 := Chr2 - 32;
      result := Chr1 - Chr2;
      if (Chr1 = 0) or (Chr2 = 0) then break;
      Str1 := Str1 + 1;
      Str2 := Str2 + 1;
      I := I + 1;
      end ;
   end ;
end ;

{  StrScan returns a PChar to the first character Chr in Str   }

function StrScan(Str: PChar; Chr: char): PChar;
var P: PChar;
begin
result := Nil;
if Str <> Nil then begin
   P := Str;
   while (P^ <> #0) and (P^ <> Chr) do
      P := P + 1;
   if P^ = Chr then result := P;
   end ;
end ;

{  StrRScan returns a PChar to the last character Chr in Str   }

function StrRScan(Str: PChar; Chr: char): PChar;
var P: PChar;
begin
result := Nil;
if Str <> Nil then begin
   P := StrEnd(Str);
   While (P^ <> Chr) and (P <> Str) do
      P := P - 1;
   If P^ = Chr then result := P;
   end ;
end ;

{  StrPos returns a PChar to the first occurance of Str2 contained in Str1
   if no occurance can be found StrPos returns Nil  }

function StrPos(Str1, Str2: PChar): PChar;
var E: PChar; Count1, Count2: Cardinal;
begin
Count1 := StrLen(Str1);
Count2 := StrLen(Str2);
if (Str1 <> Nil) and (Str2 <> Nil) and (Count1 > 0) and (Count1 >= Count2) then begin
   E := Str1 + 1 + Count1 - Count2;
   result := Str1;
   While result <> E do begin
      if StrLComp(result, Str2, Count2) = 0 then
         exit;
      result := result + 1;
      end ;
   end ;
result := Nil;
end ;

{  StrUpper converts all lowercase characters in Str to uppercase  }

function StrUpper(Str: PChar): PChar;
begin
Result := Str;
if Str <> Nil then begin
   While Str^ <> #0 do begin
      if Str^ in ['a'..'z'] then
         dec(byte(Str^), 32);
      Str := Str + 1;
      end ;
   end ;
end ;

{  StrLower converts all uppercase characters in Str to lowercase  }

function StrLower(Str: PChar): PChar;
begin
Result := Str;
if Str <> Nil then begin
   While Str^ <> #0 do begin
      if Str^ in ['A'..'Z'] then
         inc(byte(Str^), 32);
      Str := Str + 1;
      end ;
   end ;
end ;

{  StrPas converts a PChar to a pascal string  }

function StrPas(Str: PChar): string;
begin
SetLength(result, StrLen(Str));
Move(Str^, result[1], Length(result));
end ;

{  StrAlloc allocates a buffer of Size + 4
   the size of the allocated buffer is stored at result - 4
   StrDispose should be used to destroy the buffer  }

function StrAlloc(Size: cardinal): PChar;
var Temp: pointer;
begin
GetMem(Temp, Size + SizeOf(cardinal));
Move(Size, Temp^, SizeOf(cardinal));
pbyte(Temp + SizeOf(cardinal))^ := 0;
result := PChar(Temp + SizeOf(cardinal));
end ;

{  StrBufSize returns the amount of memory allocated for pchar Str allocated with StrAlloc  }

function StrBufSize(var Str: PChar): cardinal;
begin
if Str <> Nil then
   result := Cardinal(pointer(Str - SizeOf(cardinal))^)
else
   result := 0;
end ;

{  StrNew creates an exact copy of Str   }

function StrNew(Str: PChar): PChar;
begin
if Str <> Nil then begin
   result := StrAlloc(1 + StrLen(Str));
   StrCopy(result, Str);
   end
else result := Nil;
end ;

{   StrDispose clears the memory allocated with StrAlloc   }

procedure StrDispose(var Str: PChar);
var Size: cardinal;
begin
if (Str <> Nil) then begin
   Str := PChar(Str - SizeOf(cardinal));
   Move(Str^, Size, SizeOf(cardinal));
   FreeMem(Str, Size + SizeOf(cardinal));
   Str := Nil;
   end ;
end ;

{
  $Log$
  Revision 1.2  1998-09-16 08:28:40  michael
  Update from gertjan Schouten, plus small fix for linux


  1998/08/26 Gertjan
  Most functions rewritten in pascal.

  Revision 1.1  1998/04/10 15:17:46  michael
  + Initial implementation; Donated by Gertjan Schouten
    His file was split into several files, to keep it a little bit structured.


}