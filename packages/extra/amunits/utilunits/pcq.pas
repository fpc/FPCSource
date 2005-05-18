{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 2002-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$I useamigasmartlink.inc}
{$ifdef use_amiga_smartlink}
    {$smartlink on}
{$endif use_amiga_smartlink}

unit pcq;

{

     A unit to help port program from pcq pascal.

     These are some of the common C pchar functions.

     Changed a few of the functions.

     ToUpper,
     ToLower,
     strnieq,
     strieq,
     strnieq,
     stricmp
     and strnicmp

     They all use the utility.library for the checking or
     the conversion. The utility.library is opened by all
     programs as of version 1.3 of PCQ, so you don't need
     to do that.

     THIS IS CHANGED!
     Looks like the strcompare functions in utility and locale
     is buggy so I have redone this functions to use an
     internal strcompare instead.

     Added the define use_amiga_smartlink.
     13 Jan 2003.

     Changed integer > smallint.
     10 Feb 2003.

     Nils Sjoholm < nils.sjoholm@mailbox.swipnet.se

}

interface

uses exec,strings;

function CheckBreak: boolean;

Function isupper(c : Char) : Boolean;
{
    Returns True if the character is in A..Z
}

Function islower(c : Char) : Boolean;
{
    Returns True if the character is in a..z
}

Function isalpha(c : Char) : Boolean;
{
    Returns True if the character is in A..Z or a..z
}

Function isdigit(c : Char) : Boolean;
{
    Returns True if the character is in 0..9
}

Function isalnum(c : Char) : Boolean;
{
    Returns True if isalpha or isdigit is true
}

Function isspace(c : Char) : Boolean;
{
    Returns true if the character is "white space", like a space,
    form feed, line feed, carraige return, tab, whatever.
}

Function toupper(c : Char) : Char;
{
    If the character is in a..z, the function returns the capital.
    Otherwise it returns c. Not true, this function use the utility.library
    to make the conversion.
}

Function tolower(c : Char) : Char;
{
    If c is in A..Z, the function returns the lower case letter.
    Otherwise it returns c. Not true this function use the utility.library
    to make the conversion.
}

function lowercase(c : char) : char;
{
   If the character is in a..z, the function returns the capital.
   Otherwise it returns c. Not true, this function use the utility.library
   to make the conversion.
}

function lowercase(c : pchar): pchar;
{
   Will turn the pchar till lowercase.
}

function uppercase(c : char): char;
{
    If the character is in a..z, the function returns the capital.
    Otherwise it returns c. Not true, this function use the utility.library
    to make the conversion.
}

function uppercase(c: pchar): pchar;
{
    Will turn the pchar till capital letters.
}

Function streq(s1, s2 : pchar) : Boolean;
{
    Returns True if s1 and s2 are the same.
}

Function strneq(s1, s2 : pchar; n : longint) : Boolean;
{
    Returns True if the first n characters of s1 and s2 are identical.
}

Function strieq(s1, s2 : pchar) : Boolean;
{
    The same as streq(), but is case insensitive.
}

Function strnieq(s1, s2 : pchar; n : longint) : Boolean;
{
    The same as strneq(), but case insensitive.
}

Function strcmp(s1, s2 : pchar) : longint;
{
    Returns an longint < 0 if s1 < s2, zero if they are equal, and > 0
    if s1 > s2.
}

Function stricmp(s1, s2 : pchar) : longint;
{
    The same as strcmp, but not case sensitive
}

Function strncmp(s1, s2 : pchar; n : longint) : longint;
{
    Same as strcmp(), but only considers the first n characters.
}

Function strnicmp(s1, s2 : pchar; n : longint) : longint;
{
    Same as strncmp, but not case sensitive
}

Procedure strcpy(s1, s2 : pchar);
{
    Copies s2 into s1, appending a trailing zero.  This is the same
    as C, but opposite from 1.0.
}
Procedure strncpy(s1, s2 : pchar; n : smallint);
{
    Copies s2 into s1, with a maximum of n characters.  Appends a
    trailing zero.
}

Procedure strncat(s1, s2 : pchar; n : smallint);
{
    Appends at most n characters from s2 onto s1.
}

Function strdup(s : pchar) : pchar;
{
    This allocates a copy of the pchar 's', and returns a ptr
}

Function strpos(s1 : pchar; c : Char) : longint;
{
    Return the position, starting at zero, of the first (leftmost)
    occurance of c in s1.  If there is no c, it returns -1.
}

Function strrpos(s1 : pchar; c : Char) : longint;
{
    Returns the longint position of the right-most occurance of c in s1.
    If c is not in s1, it returns -1.
}

Function AllocString(l : longint) : pchar;
{
    Allocates l bytes, and returns a pointer to the allocated memory.
This memory is allocated through the new() function, so it will be returned
to the system at the end of your program.  Note that the proper amount of RAM
to allocate is strlen(s) + 1.
}

Procedure FreeString(s : pchar);
{
    This returns memory allocated by AllocString to the system.  Since
the Amiga is a multitasking computer, you should always return memory you
don't need to the system.
}

implementation

const
     SIGBREAKF_CTRL_C = $1000;

function CheckBreak: boolean;
begin
   { check for Ctrl-C break by user }
   if (Setsignal(0,0) AND SIGBREAKF_CTRL_C) <> 0 then Begin
       SetSignal(0,SIGBREAKF_CTRL_C);
       CheckBreak := true;
   end else CheckBreak := false;
end;

Function isupper(c : Char) : Boolean;
begin
     if ((ord(c) >= 192) and (ord(c) <= 223)) or ((c >= 'A') and (c <= 'Z'))
         then isupper := true
     else isupper := false;
end;

Function islower(c : Char) : Boolean;
begin
     if ((ord(c) >= 224) and (ord(c) <= 254)) or ((c >= 'a') and (c <= 'z'))
         then islower := true
     else islower := false;
end;

Function isalpha(c : Char) : Boolean;
begin
     if ((ord(c) >= 192) and (ord(c) <= 223)) or ((c >= 'A') and (c <= 'Z'))
     or ((ord(c) >= 224) and (ord(c) <= 254)) or ((c >= 'a') and (c <= 'z'))
         then isalpha := true
     else isalpha := false;
end;

Function isdigit(c : Char) : Boolean;
begin
     if c in ['0'..'9'] then isdigit := true
     else isdigit := false;
end;

Function isalnum(c : Char) : Boolean;
begin
     if isalpha(c) or isdigit(c) then isalnum := true
     else isalnum := false;
end;

Function isspace(c : Char) : Boolean;
begin
     if c in [#9..#13,#32] then isspace := true
     else isspace := false;
end;

Function toupper(c : Char) : Char;
begin
    if ((ord(c) >= 224) and (ord(c) <= 254)) or ((c >= 'a') and (c <= 'z'))
        then c := char(ord(c)-32);
    toupper := c;
end;

Function tolower(c : Char) : Char;
begin
    if ((ord(c) >= 192) and (ord(c) <= 223)) or ((c >= 'A') and (c <= 'Z'))
        then c := char(ord(c)+32);
    tolower := c;
end;

function lowercase(c : char) : char;
begin
    lowercase := tolower(c);
end;

function lowercase(c : pchar): pchar;
var
    i : longint;
begin
    i := 0;
    while c[i] <> #0 do begin
        c[i] := tolower(c[i]);
        i := succ(i);
    end;
    lowercase := c;
end;

function uppercase(c : char): char;
begin
    uppercase := toupper(c);
end;

function uppercase(c: pchar): pchar;
var
    i : longint;
begin
    i := 0;
    while c[i] <> #0 do begin
        c[i] := toupper(c[i]);
        i := succ(i);
    end;
    uppercase := c;
end;

Function streq(s1, s2 : pchar) : Boolean;
begin
    streq := (strcomp(s1,s2) = 0);
end;

Function strneq(s1, s2 : pchar; n : longint) : Boolean;
begin
    strneq := (strlcomp(s1,s2,n) = 0);
end;

Function strieq(s1, s2 : pchar) : Boolean;
begin
    s1 := uppercase(s1);
    s2 := uppercase(s2);
    strieq := (strcomp(s1,s2)=0);
end;

Function strnieq(s1, s2 : pchar; n : longint) : Boolean;
begin
    s1 := uppercase(s1);
    s2 := uppercase(s2);
    strnieq := (strlcomp(s1,s2,n)=0);
end;

Function strcmp(s1, s2 : pchar) : longint;
begin
    strcmp := strcomp(s1,s2);
end;

Function stricmp(s1, s2 : pchar) : longint;
begin
    s1 := uppercase(s1);
    s2 := uppercase(s2);
    stricmp := strcomp(s1,s2);
end;

Function strncmp(s1, s2 : pchar; n : longint) : longint;
begin
    strncmp := strlcomp(s1,s2,n);
end;

Function strnicmp(s1, s2 : pchar; n : longint) : longint;
begin
    s1 := uppercase(s1);
    s2 := uppercase(s2);
    strnicmp := strlcomp(s1,s2,n);
end;

Procedure strcpy(s1, s2 : pchar);
begin
    strcopy(s1,s2)
end;

Procedure strncpy(s1, s2 : pchar; n : smallint);
begin
    strlcopy(s1,s2,n);
end;

Procedure strncat(s1, s2 : pchar; n : smallint);
begin
    strlcat(s1,s2,n);
end;

Function strdup(s : pchar) : pchar;
begin
    strdup := StrNew(s);
end;

Function strpos(s1 : pchar; c : Char) : longint;
  Var
     count: Longint;
  Begin

   count := 0;
   { As in Borland Pascal , if looking for NULL return null }
   if c = #0 then
   begin
     strpos := -1;
     exit;
   end;
   { Find first matching character of Ch in Str }
   while S1[count] <> #0 do
   begin
     if C = S1[count] then
      begin
          strpos := count;
          exit;
      end;
     Inc(count);
   end;
   { nothing found. }
   strpos := -1;
 end;


Function strrpos(s1 : pchar; c : Char) : longint;
Var
  count: Longint;
  index: Longint;
 Begin
   count := Strlen(S1);
   { As in Borland Pascal , if looking for NULL return null }
   if c = #0 then
   begin
     strrpos := -1;
     exit;
   end;
   Dec(count);
   for index := count downto 0 do
   begin
     if C = S1[index] then
      begin
          strrpos := index;
          exit;
      end;
   end;
   { nothing found. }
   strrpos := -1;
 end;


Function AllocString(l : longint) : pchar;
begin
    AllocString := StrAlloc(l);
end;

Procedure FreeString(s : pchar);
begin
    StrDispose(s);
end;

end.
