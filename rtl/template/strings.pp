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
Unit Strings;


  {*********************************************************************}
  { Strings unit, 100% portable.                                        }
  {- COMPILING INFORMATION ---------------------------------------------}
  {   The only difference between this  unit and the one supplied with  }
  {   Turbo Pascal 7.01, are that StrLen returns a longint, and the     }
  {   routines requiring a length now use longints instead of words.    }
  {   This should not influence the behaviour of your programs under    }
  {   Turbo Pascal. (it will even create better error checking for your }
  {   programs).                                                        }
  {*********************************************************************}

 Interface
 {*********************************************************************}
 { Returns the number of Characters in Str,not counting the Null       }
 { chracter.                                                           }
 {*********************************************************************}

function StrLen(Str: PChar): longint;


function StrEnd(Str: PChar): PChar;

  {*********************************************************************}
  {  Description: Move count characters from source to dest.            }
  {   Do not forget to use StrLen(source)+1 as l parameter to also move }
  {   the null character.                                               }
  {  Return value: Dest                                                 }
  {   Remarks: Source and Dest may overlap.                             }
  {*********************************************************************}

function StrMove(Dest,Source : Pchar;l : Longint) : pchar;


function StrCopy(Dest, Source: PChar): PChar;

 {*********************************************************************}
 {  Input: Source -> Source of the null-terminated string to copy.     }
 {         Dest   -> Destination of null terminated string to copy.    }
 {    Return Value: Pointer to the end of the copied string of Dest.   }
 {  Output: Dest ->   Pointer to the copied string.                    }
 {*********************************************************************}
function StrECopy(Dest, Source: PChar): PChar;

  {*********************************************************************}
  {  Copies at most MaxLen characters from Source to Dest.              }
  {                                                                     }
  {   Remarks: According to the Turbo Pascal programmer's Reference     }
  {    this routine performs length checking. From the code of the      }
  {    original strings unit, this does not seem true...                }
  {   Furthermore, copying a null string gives two null characters in   }
  {   the destination according to the Turbo Pascal routine.            }
  {*********************************************************************}

function StrLCopy(Dest, Source: PChar; MaxLen: Longint): PChar;

 {*********************************************************************}
 {  Input: Source -> Source of the pascal style string to copy.        }
 {         Dest   -> Destination of null terminated string to copy.    }
 {    Return Value: Dest. (with noew copied string)                    }
 {*********************************************************************}

function StrPCopy(Dest: PChar; Source: String): PChar;

 {*********************************************************************}
 {  Description: Appends a copy of Source to then end of Dest and      }
 {               return Dest.                                          }
 {*********************************************************************}

function StrCat(Dest, Source: PChar): PChar;

 {*********************************************************************}
 { Description: Appends at most MaxLen - StrLen(Dest) characters from  }
 { Source to the end of Dest, and returns Dest.                        }
 {*********************************************************************}

      function strlcat(dest,source : pchar;l : Longint) : pchar;

  {*********************************************************************}
  {  Compares two strings. Does the ASCII value substraction of the     }
  {  first non matching characters                                      }
  {   Returns 0 if both strings are equal                               }
  {   Returns < 0 if Str1 < Str2                                        }
  {   Returns > 0 if Str1 > Str2                                        }
  {*********************************************************************}

function StrComp(Str1, Str2: PChar): Integer;

  {*********************************************************************}
  {  Compares two strings without case sensitivity. See StrComp for more}
  {  information.                                                       }
  {   Returns 0 if both strings are equal                               }
  {   Returns < 0 if Str1 < Str2                                        }
  {   Returns > 0 if Str1 > Str2                                        }
  {*********************************************************************}

function StrIComp(Str1, Str2: PChar): Integer;

  {*********************************************************************}
  {  Compares two strings up to a maximum of MaxLen characters.         }
  {                                                                     }
  {   Returns 0 if both strings are equal                               }
  {   Returns < 0 if Str1 < Str2                                        }
  {   Returns > 0 if Str1 > Str2                                        }
  {*********************************************************************}

function StrLComp(Str1, Str2: PChar; MaxLen: Longint): Integer;

  {*********************************************************************}
  {  Compares two strings up to a maximum of MaxLen characters.         }
  {  The comparison is case insensitive.                                }
  {   Returns 0 if both strings are equal                               }
  {   Returns < 0 if Str1 < Str2                                        }
  {   Returns > 0 if Str1 > Str2                                        }
  {*********************************************************************}

function StrLIComp(Str1, Str2: PChar; MaxLen: Longint): Integer;

 {*********************************************************************}
 {  Input: Str  -> String to search.                                   }
 {         Ch   -> Character to find in Str.                           }
 {  Return Value: Pointer to first occurence of Ch in Str, nil if      }
 {                not found.                                           }
 {  Remark: The null terminator is considered being part of the string }
 {*********************************************************************}

function StrScan(Str: PChar; Ch: Char): PChar;

 {*********************************************************************}
 {  Input: Str  -> String to search.                                   }
 {         Ch   -> Character to find in Str.                           }
 {  Return Value: Pointer to last occurence of Ch in Str, nil if       }
 {                not found.                                           }
 {  Remark: The null terminator is considered being part of the string }
 {*********************************************************************}


function StrRScan(Str: PChar; Ch: Char): PChar;

 {*********************************************************************}
 {  Input: Str1 -> String to search.                                   }
 {         Str2 -> String to match in Str1.                            }
 {  Return Value: Pointer to first occurence of Str2 in Str1, nil if   }
 {                not found.                                           }
 {*********************************************************************}

function StrPos(Str1, Str2: PChar): PChar;

 {*********************************************************************}
 {  Input: Str -> null terminated string to uppercase.                 }
 {  Output:Str -> null terminated string in upper case characters.     }
 {    Return Value: null terminated string in upper case characters.   }
 {  Remarks: Case conversion is dependant on upcase routine.           }
 {*********************************************************************}

function StrUpper(Str: PChar): PChar;

 {*********************************************************************}
 {  Input: Str -> null terminated string to lower case.                }
 {  Output:Str -> null terminated string in lower case characters.     }
 {    Return Value: null terminated string in lower case characters.   }
 {  Remarks: Only converts standard ASCII characters.                  }
 {*********************************************************************}

function StrLower(Str: PChar): PChar;

{ StrPas converts Str to a Pascal style string.                 }

function StrPas(Str: PChar): String;

 {*********************************************************************}
 {  Input: Str  -> String to duplicate.                                }
 {  Return Value: Pointer to the new allocated string. nil if no       }
 {                  string allocated. If Str = nil then return value   }
 {                  will also be nil (in this case, no allocation      }
 {                  occurs). The size allocated is of StrLen(Str)+1    }
 {                  bytes.                                             }
 {*********************************************************************}
function StrNew(P: PChar): PChar;

{ StrDispose disposes a string that was previously allocated    }
{ with StrNew. If Str is NIL, StrDispose does nothing.          }

procedure StrDispose(P: PChar);

Implementation


 function strlen(Str : pchar) : Longint;
  var
   counter : Longint;
 Begin
   counter := 0;
   while Str[counter] <> #0 do
     Inc(counter);
   strlen := counter;
 end;



 Function strpas(Str: pchar): string;
 { only 255 first characters are actually copied. }
  var
   counter : byte;
   lstr: string;
 Begin
   counter := 0;
   lstr := '';
   while (ord(Str[counter]) <> 0) and (counter < 255) do
   begin
     Inc(counter);
     lstr[counter] := char(Str[counter-1]);
   end;
   lstr[0] := char(counter);
   strpas := lstr;
 end;

 Function StrEnd(Str: PChar): PChar;
 var
  counter: Longint;
 begin
   counter := 0;
   while Str[counter] <> #0 do
      Inc(counter);
   StrEnd := @(Str[Counter]);
 end;


 Function StrCopy(Dest, Source:PChar): PChar;
 var
   counter : Longint;
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


 function StrCat(Dest,Source: PChar): PChar;
 var
  counter: Longint;
  PEnd: PChar;
 begin
   PEnd := StrEnd(Dest);
   counter := 0;
   while (Source[counter] <> #0) do
   begin
     PEnd[counter] := char(Source[counter]);
     Inc(counter);
   end;
   { terminate the string }
   PEnd[counter] := #0;
   StrCat := Dest;
 end;

 function StrUpper(Str: PChar): PChar;
 var
  counter: Longint;
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

 function StrLower(Str: PChar): PChar;
 var
  counter: Longint;
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


  function StrPos(Str1,Str2: PChar): PChar;
 var
  count: Longint;
  oldindex: Longint;
  found: boolean;
  Str1Length: Longint;
  Str2Length: Longint;
  ll: Longint;
 Begin

   Str1Length := StrLen(Str1);
   Str2Length := StrLen(Str2);
   found := true;
   oldindex := 0;

   { If the search string is greater than the string to be searched }
   { it is certain that we will not find it.                        }
   { Furthermore looking for a null will simply give out a pointer, }
   { to the null character of str1 as in Borland Pascal.            }
   if (Str2Length > Str1Length) or (Str2[0] = #0) then
   begin
     StrPos := nil;
     exit;
   end;

   Repeat
     { Find first matching character of Str2 in Str1 }
     { put index of this character in oldindex       }
     for count:= oldindex to Str1Length-1 do
     begin
        if Str2[0] = Str1[count] then
        begin
           oldindex := count;
           break;
        end;
        { nothing found - exit routine }
        if count = Str1Length-1 then
        begin
           StrPos := nil;
           exit;
        end;
     end;

     found := true;
     { Compare the character strings }
     { and check if they match.      }
     for ll := 0 to Str2Length-1 do
     begin
       { no match, stop iteration }
        if (Str2[ll] <> Str1[ll+oldindex]) then
        begin
           found := false;
           break;
        end;
     end;
     { Not found, the index will no point at next character }
     if not found then
       Inc(oldindex);
     { There was a match }
     if found then
     begin
        StrPos := @(Str1[oldindex]);
        exit;
     end;
   { If we have gone through the whole string to search }
   { then exit routine.                                 }
   Until (Str1Length-oldindex) <= 0;
   StrPos := nil;
 end;


 function StrScan(Str: PChar; Ch: Char): PChar;
   Var
     count: Longint;
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



 function StrRScan(Str: PChar; Ch: Char): PChar;
 Var
  count: Longint;
  index: Longint;
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


 function StrNew(p:PChar): PChar;
      var
         len : Longint;
         tmp : pchar;
      begin
         strnew:=nil;
         if (p=nil) or (p^=#0) then
           exit;
         len:=strlen(p)+1;
         getmem(tmp,len);
         if tmp<>nil then
           strmove(tmp,p,len);
         StrNew := tmp;
      end;


  Function StrECopy(Dest, Source: PChar): PChar;
 { Equivalent to the following:                                          }
 {  strcopy(Dest,Source);                                                }
 {  StrECopy := StrEnd(Dest);                                            }
 var
   counter : Longint;
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


   Function StrPCopy(Dest: PChar; Source: String):PChar;
   var
    counter : byte;
  Begin
    counter := 0;
   { if empty pascal string  }
   { then setup and exit now }
   if Source = '' then
   Begin
     Dest[0] := #0;
     StrPCopy := Dest;
     exit;
   end;
   for counter:=1 to length(Source) do
   begin
     Dest[counter-1] := Source[counter];
   end;
   { terminate the string }
   Dest[counter] := #0;
   StrPCopy:=Dest;
 end;


 procedure strdispose(p : pchar);
 begin
   if p<>nil then
      freemem(p,strlen(p)+1);
 end;


 function strmove(dest,source : pchar;l : Longint) : pchar;
 begin
   move(source^,dest^,l);
   strmove:=dest;
 end;


 function strlcat(dest,source : pchar;l : Longint) : pchar;
 var
   destend : pchar;
 begin
   destend:=strend(dest);
   l:=l-(destend-dest);
   strlcat:=strlcopy(destend,source,l);
 end;


 Function StrLCopy(Dest,Source: PChar; MaxLen: Longint): PChar;
  var
   counter: Longint;
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


 function StrComp(Str1, Str2 : PChar): Integer;
     var
      counter: Longint;
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

     function StrIComp(Str1, Str2 : PChar): Integer;
     var
      counter: Longint;
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


     function StrLComp(Str1, Str2 : PChar; MaxLen: Longint): Integer;
     var
      counter: Longint;
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



     function StrLIComp(Str1, Str2 : PChar; MaxLen: Longint): Integer;
     var
      counter: Longint;
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
end.
{
  $Log$
  Revision 1.3  2002-09-07 16:01:27  peter
    * old logs removed and tabs fixed

}
