{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl-Eric Codere,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_UNIT_HAS_STRLEN}
 function strlen(Str : pchar) : StrLenInt;
  var
   counter : StrLenInt;
 Begin
   counter := 0;
   while Str[counter] <> #0 do
     Inc(counter);
   strlen := counter;
 end;
{$endif FPC_UNIT_HAS_STRLEN}


{$ifndef FPC_UNIT_HAS_STREND}
 Function StrEnd(Str: PChar): PChar;
 var
  counter: StrLenInt;
 begin
   counter := 0;
   while Str[counter] <> #0 do
      Inc(counter);
   StrEnd := @(Str[Counter]);
 end;
{$endif FPC_UNIT_HAS_STREND}


{$ifndef FPC_UNIT_HAS_STRCOPY}
 Function StrCopy(Dest, Source:PChar): PChar;
 var
   counter : StrLenInt;
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



{$ifndef FPC_UNIT_HAS_UPPER}
 function StrUpper(Str: PChar): PChar;
 var
  counter: StrLenInt;
 begin
   counter := 0;
   while (Str[counter] <> #0) do
   begin
     if Str[Counter] in [#97..#122,#128..#255] then
        Str[counter] := Upcase(Str[counter]);
     Inc(counter);
   end;
   StrUpper := Str;
 end;
{$endif FPC_UNIT_HAS_UPPER}


{$ifndef FPC_UNIT_HAS_LOWER}
 function StrLower(Str: PChar): PChar;
 var
  counter: StrLenInt;
 begin
   counter := 0;
   while (Str[counter] <> #0) do
   begin
     if Str[counter] in [#65..#90] then
        Str[Counter] := chr(ord(Str[Counter]) + 32);
     Inc(counter);
   end;
   StrLower := Str;
 end;
{$endif FPC_UNIT_HAS_LOWER}



{$ifndef FPC_UNIT_HAS_STRSCAN}
 function StrScan(Str: PChar; Ch: Char): PChar;
   Var
     count: StrLenInt;
  Begin

   count := 0;
   { As in Borland Pascal , if looking for NULL return null }
   if ch = #0 then
   begin
     StrScan := @(Str[StrLen(Str)]);
     exit;
   end;
   { Find first matching character of Ch in Str }
   while Str[count] <> #0 do
   begin
     if Ch = Str[count] then
      begin
          StrScan := @(Str[count]);
          exit;
      end;
     Inc(count);
   end;
   { nothing found. }
   StrScan := nil;
 end;
{$endif FPC_UNIT_HAS_STRSCAN}



{$ifndef FPC_UNIT_HAS_STRRSCAN}
 function StrRScan(Str: PChar; Ch: Char): PChar;
 Var
  count: StrLenInt;
  index: StrLenInt;
 Begin
   count := Strlen(Str);
   { As in Borland Pascal , if looking for NULL return null }
   if ch = #0 then
   begin
     StrRScan := @(Str[count]);
     exit;
   end;
   Dec(count);
   for index := count downto 0 do
   begin
     if Ch = Str[index] then
      begin
          StrRScan := @(Str[index]);
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
   counter : StrLenInt;
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
 Function StrLCopy(Dest,Source: PChar; MaxLen: StrLenInt): PChar;
  var
   counter: StrLenInt;
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
 function StrComp(Str1, Str2 : PChar): Integer;
     var
      counter: StrLenInt;
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
     function StrIComp(Str1, Str2 : PChar): Integer;
     var
      counter: StrLenInt;
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
     function StrLComp(Str1, Str2 : PChar; MaxLen: StrLenInt): Integer;
     var
      counter: StrLenInt;
      c1, c2: char;
     Begin
        counter := 0;
       if MaxLen = 0 then
       begin
         StrLComp := 0;
         exit;
       end;
       Repeat
         if (c1 = #0) or (c2 = #0) then break;
         c1 := str1[counter];
         c2 := str2[counter];
         Inc(counter);
      Until (c1 <> c2) or (counter >= MaxLen);
       StrLComp := ord(c1) - ord(c2);
     end;
{$endif FPC_UNIT_HAS_STRLCOMP}


{$ifndef FPC_UNIT_HAS_STRLICOMP}
     function StrLIComp(Str1, Str2 : PChar; MaxLen: StrLenInt): Integer;
     var
      counter: StrLenInt;
      c1, c2: char;
     Begin
        counter := 0;
       if MaxLen = 0 then
       begin
         StrLIComp := 0;
         exit;
       end;
       Repeat
         if (c1 = #0) or (c2 = #0) then break;
         c1 := upcase(str1[counter]);
         c2 := upcase(str2[counter]);
         Inc(counter);
      Until (c1 <> c2) or (counter >= MaxLen);
       StrLIComp := ord(c1) - ord(c2);
     end;
{$endif FPC_UNIT_HAS_STRLICOMP}

{
  $Log$
  Revision 1.1  2003-04-30 16:36:39  florian
    + support for generic pchar routines added
    + some basic rtl stuff for x86-64 added
}
