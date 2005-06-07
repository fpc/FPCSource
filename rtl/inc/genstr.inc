{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl-Eric Codere,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_UNIT_HAS_STREND}
 Function StrEnd(P: PChar): PChar;
 var
  counter: SizeInt;
 begin
   counter := 0;
   while P[counter] <> #0 do
      Inc(counter);
   StrEnd := @(P[Counter]);
 end;
{$endif FPC_UNIT_HAS_STREND}


{$ifndef FPC_UNIT_HAS_STRCOPY}
 Function StrCopy(Dest, Source:PChar): PChar;
 var
   counter : SizeInt;
 Begin
   counter := 0;
   while Source[counter] <> #0 do
   begin
     Dest[counter] := char(Source[counter]);
     Inc(counter);
   end;
   { terminate the string }
   Dest[counter] := #0;
   StrCopy := Dest;
 end;
{$endif FPC_UNIT_HAS_STRCOPY}



{$ifndef FPC_UNIT_HAS_STRUPPER}
 function StrUpper(P: PChar): PChar;
 var
  counter: SizeInt;
 begin
   counter := 0;
   while (P[counter] <> #0) do
   begin
     if P[Counter] in [#97..#122,#128..#255] then
        P[counter] := Upcase(P[counter]);
     Inc(counter);
   end;
   StrUpper := P;
 end;
{$endif FPC_UNIT_HAS_STRUPPER}


{$ifndef FPC_UNIT_HAS_STRLOWER}
 function StrLower(P: PChar): PChar;
 var
  counter: SizeInt;
 begin
   counter := 0;
   while (P[counter] <> #0) do
   begin
     if P[counter] in [#65..#90] then
        P[Counter] := chr(ord(P[Counter]) + 32);
     Inc(counter);
   end;
   StrLower := P;
 end;
{$endif FPC_UNIT_HAS_STRLOWER}



{$ifndef FPC_UNIT_HAS_STRSCAN}
 function StrScan(P: PChar; C: Char): PChar;
   Var
     count: SizeInt;
  Begin

   count := 0;
   { As in Borland Pascal , if looking for NULL return null }
   if C = #0 then
   begin
     StrScan := @(P[StrLen(P)]);
     exit;
   end;
   { Find first matching character of Ch in Str }
   while P[count] <> #0 do
   begin
     if C = P[count] then
      begin
          StrScan := @(P[count]);
          exit;
      end;
     Inc(count);
   end;
   { nothing found. }
   StrScan := nil;
 end;
{$endif FPC_UNIT_HAS_STRSCAN}



{$ifndef FPC_UNIT_HAS_STRRSCAN}
 function StrRScan(P: PChar; C: Char): PChar;
 Var
  count: SizeInt;
  index: SizeInt;
 Begin
   count := Strlen(P);
   { As in Borland Pascal , if looking for NULL return null }
   if C = #0 then
   begin
     StrRScan := @(P[count]);
     exit;
   end;
   Dec(count);
   for index := count downto 0 do
   begin
     if C = P[index] then
      begin
          StrRScan := @(P[index]);
          exit;
      end;
   end;
   { nothing found. }
   StrRScan := nil;
 end;
{$endif FPC_UNIT_HAS_STRRSCAN}


{$ifndef FPC_UNIT_HAS_STRECOPY}
  Function StrECopy(Dest, Source: PChar): PChar;
 { Equivalent to the following:                                          }
 {  strcopy(Dest,Source);                                                }
 {  StrECopy := StrEnd(Dest);                                            }
 var
   counter : SizeInt;
 Begin
   counter := 0;
   while Source[counter] <> #0 do
   begin
     Dest[counter] := char(Source[counter]);
     Inc(counter);
   end;
   { terminate the string }
   Dest[counter] := #0;
   StrECopy:=@(Dest[counter]);
 end;
{$endif FPC_UNIT_HAS_STRECOPY}


{$ifndef FPC_UNIT_HAS_STRLCOPY}
 Function StrLCopy(Dest,Source: PChar; MaxLen: SizeInt): PChar;
  var
   counter: SizeInt;
 Begin
   counter := 0;
   { To be compatible with BP, on a null string, put two nulls }
   If Source[0] = #0 then
   Begin
     Dest[0]:=Source[0];
     Inc(counter);
   end;
   while (Source[counter] <> #0)  and (counter < MaxLen) do
   Begin
      Dest[counter] := char(Source[counter]);
      Inc(counter);
   end;
   { terminate the string }
   Dest[counter] := #0;
   StrLCopy := Dest;
 end;
{$endif FPC_UNIT_HAS_STRLCOPY}


{$ifndef FPC_UNIT_HAS_STRCOMP}
 function StrComp(Str1, Str2 : PChar): SizeInt;
     var
      counter: SizeInt;
     Begin
        counter := 0;
       While str1[counter] = str2[counter] do
       Begin
         if (str2[counter] = #0) or (str1[counter] = #0) then
            break;
         Inc(counter);
       end;
       StrComp := ord(str1[counter]) - ord(str2[counter]);
     end;
{$endif FPC_UNIT_HAS_STRCOMP}


{$ifndef FPC_UNIT_HAS_STRICOMP}
     function StrIComp(Str1, Str2 : PChar): SizeInt;
     var
      counter: SizeInt;
      c1, c2: char;
     Begin
        counter := 0;
        c1 := upcase(str1[counter]);
        c2 := upcase(str2[counter]);
       While c1 = c2 do
       Begin
         if (c1 = #0) or (c2 = #0) then break;
         Inc(counter);
         c1 := upcase(str1[counter]);
         c2 := upcase(str2[counter]);
      end;
       StrIComp := ord(c1) - ord(c2);
     end;
{$endif FPC_UNIT_HAS_STRICOMP}


{$ifndef FPC_UNIT_HAS_STRLCOMP}
     function StrLComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;
     var
      counter: SizeInt;
      c1, c2: char;
     Begin
        counter := 0;
       if L = 0 then
       begin
         StrLComp := 0;
         exit;
       end;
       Repeat
         c1 := str1[counter];
         c2 := str2[counter];
         if (c1 = #0) or (c2 = #0) then break;
         Inc(counter);
      Until (c1 <> c2) or (counter >= L);
       StrLComp := ord(c1) - ord(c2);
     end;
{$endif FPC_UNIT_HAS_STRLCOMP}


{$ifndef FPC_UNIT_HAS_STRLICOMP}
     function StrLIComp(Str1, Str2 : PChar; L: SizeInt): SizeInt;
     var
      counter: SizeInt;
      c1, c2: char;
     Begin
        counter := 0;
       if L = 0 then
       begin
         StrLIComp := 0;
         exit;
       end;
       Repeat
         c1 := upcase(str1[counter]);
         c2 := upcase(str2[counter]);
         if (c1 = #0) or (c2 = #0) then break;
         Inc(counter);
      Until (c1 <> c2) or (counter >= L);
       StrLIComp := ord(c1) - ord(c2);
     end;
{$endif FPC_UNIT_HAS_STRLICOMP}

