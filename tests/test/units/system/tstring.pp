{ Program to test system unit string routines
  Tested against Delphi 3 and (where possible)
  against Borland Pascal v7.01
}
program tstring;
{$R+}
{$Q+}

{$ifndef MACOS}
{$APPTYPE CONSOLE}
{$else}
{$APPTYPE TOOL}
{$endif}

{$ifdef fpc}
  {$ifndef ver1_0}
    {$define haswidestring}
  {$endif}
{$else}
  {$ifndef ver70}
    {$define haswidestring}
  {$endif}
{$endif}

var
   str1 : shortstring;
   str2 : ansistring;
{$ifdef haswidestring}
   str3 : widestring;
{$endif}


procedure fail;
 begin
   WriteLn('Failed!');
   Halt(1);
 end;


procedure test_stringofchar;
 var
   _result : boolean;
   i: integer;
 begin
   Write('StringOfChar tests...');
   _result := true;
   {************************* shortstring ************************}
   { try to fill a shortstring with a null character }
   str1:='';
   str1:=stringofchar(#0,0);
   if length(str1)<>0 then
     _result := false;
   str1:='';

   str1:='';
   str1:=stringofchar('a',-1);
   if length(str1)<>0 then
     _result := false;
   str1:='';


   { try to fill a shortstring with more chars than possible }
   str1:=stringofchar('c',300);
   if length(str1)<>255 then
     _result := false;
   { try to fill a shortstring with no chars }
   str1:='';
   str1:=stringofchar('c',0);
   if length(str1)<>0 then
     _result := false;
   { try to fill a shortstring chars }
   str1:='';
   str1:=stringofchar('a',255);
   for i:=1 to 255 do
     if str1[i] <> 'a' then
        _result := false;
   {************************* ansistring *************************}
   { try to fill a ansistring with a null character }
   str2:='';
   str2:=stringofchar(#0,0);
   if length(str2)<>0 then
     _result := false;

   str2:='';
   str2:=stringofchar('a',-1);
   if length(str2)<>0 then
     _result := false;

   { try to fill a ansistring with no chars }
   str2:='';
   str2:=stringofchar('c',0);
   if length(str2)<>0 then
     _result := false;
   { try to fill an ansistring chars }
   str2:='';
   str2:=stringofchar('a',1024);
   for i:=1 to 1024 do
     if str2[i] <> 'a' then
        _result := false;
   {************************* widestring *************************}
{$ifdef haswidestring}
   { try to fill a widestring with a null character }
   str3:='';
   str3:=stringofchar(#0,0);
   if length(str3)<>0 then
     _result := false;
   str3:='';
   { try to fill a widestring with no chars }
   str3:='';
   str3:=stringofchar('c',0);
   if length(str3)<>0 then
     _result := false;
   { try to fill an widestring chars }
   str3:='';
   str3:=stringofchar('a',1024);
   for i:=1 to 1024 do
     if str3[i] <> 'a' then
        _result := false;

   str3:='';
   str3:=stringofchar('a',-1);
   if length(str3)<>0 then
     _result := false;

{$endif}
   if not _result then
      fail
   else
     WriteLn('Success!');
 end;


 procedure test_delete;
 var
   _result : boolean;
   i: integer;
 begin
   Write('Delete tests...');
   _result := true;
   {************************* shortstring ************************}
   { try to delete from an empty string }
   str1:='';
   Delete(str1,0,12);
   if str1<>'' then
     _result := false;

   str1:='Hello';
   Delete(str1,0,12);
   if str1<>'Hello' then
     _result := false;

   str1:='Hello';
   Delete(str1,1,12);
   if str1<>'' then
     _result := false;

   str1:='Hello';
   Delete(str1,12,255);
   if str1<>'Hello' then
     _result := false;

   str1:='Hello';
   Delete(str1,-1,255);
   if str1<>'Hello' then
     _result := false;

   str1:='Hello';
   Delete(str1,1,-12);
   if str1<>'Hello' then
     _result := false;

   {************************* ansistring *************************}
   { try to delete from an empty string }
   str2:='';
   Delete(str2,0,12);
   if str2<>'' then
     _result := false;

   str2:='Hello';
   Delete(str2,0,12);
   if str2<>'Hello' then
     _result := false;

   str2:='Hello';
   Delete(str2,1,12);
   if str2<>'' then
     _result := false;

   str2:='Hello';
   Delete(str2,12,255);
   if str2<>'Hello' then
     _result := false;

   STR2:='Hello';
   Delete(STR2,-1,255);
   if STR2<>'Hello' then
     _result := false;

   STR2:='Hello';
   Delete(STR2,1,-12);
   if STR2<>'Hello' then
     _result := false;

   {************************* widestring *************************}
{$ifdef haswidestring}
   { try to delete from an empty string }
   str3:='';
   Delete(str3,0,12);
   if str3<>'' then
     _result := false;

   str3:='Hello';
   Delete(str3,0,12);
   if str3<>'Hello' then
     _result := false;

   str3:='Hello';
   Delete(str3,1,12);
   if str3<>'' then
     _result := false;

   str3:='Hello';
   Delete(str3,12,255);
   if str3<>'Hello' then
     _result := false;

   str3:='Hello';
   Delete(str3,-1,255);
   if str3<>'Hello' then
     _result := false;

   str3:='Hello';
   Delete(str3,1,-12);
   if str3<>'Hello' then
     _result := false;

{$endif}
   if not _result then
      fail
   else
     WriteLn('Success!');
 end;

 procedure test_copy;
  var
    _result : boolean;
    i: integer;
  begin
    Write('Copy tests...');
    _result := true;

   {************************* shortstring ************************}
   { try to copy from an empty string }
   str1:='';
   str1:=Copy(str1,1,12);
   if str1<>'' then
     _result := false;

   str1:='';
   str1:=Copy('Hello world',0,12);
   if str1<>'Hello world' then
     _result := false;

   str1:='';
   str1:=Copy('Hello world',1,12);
   if str1<>'Hello world' then
     _result := false;

   str1:='';
   str1:=Copy('Hello world',-12,12);
   if str1<>'Hello world' then
     _result := false;

   str1:='';
   str1:=Copy('Hello world',64,128);
   if str1<>'' then
     _result := false;

   str1:='';
   str1:=Copy('Hello world',1,-12);
   if str1<>'' then
     _result := false;

   str1:='';
   str1:=Copy('Hello world',-12,0);
   if str1<>'' then
     _result := false;

   str1:='';
   str1:=Copy('Hello world',7,11);
   if str1<>'world' then
     _result := false;

   str1:='';
   str1:=Copy('Hello world',1,11);
   if str1<>'Hello world' then
     _result := false;

   str1:='';
   str1:=Copy('',0,12);
   if str1<>'' then
     _result := false;

   {************************* ansistring *************************}
   { try to copy from an empty string }
   str2:='';
   str2:=Copy(str2,1,12);
   if str2<>'' then
     _result := false;

   str2:='';
   str2:=Copy('Hello world',0,12);
   if str2<>'Hello world' then
     _result := false;

   str2:='';
   str2:=Copy('Hello world',1,12);
   if str2<>'Hello world' then
     _result := false;

   str2:='';
   str2:=Copy('Hello world',-12,12);
   if str2<>'Hello world' then
     _result := false;

   str2:='';
   str2:=Copy('Hello world',64,128);
   if str2<>'' then
     _result := false;

   str2:='';
   str2:=Copy('Hello world',1,-12);
   if str2<>'' then
     _result := false;

   str2:='';
   str2:=Copy('Hello world',-12,0);
   if str2<>'' then
     _result := false;

   str2:='';
   str2:=Copy('Hello world',7,11);
   if str2<>'world' then
     _result := false;

   str2:='';
   str2:=Copy('Hello world',1,11);
   if str2<>'Hello world' then
     _result := false;

   str2:='';
   str2:=Copy('',0,12);
   if str2<>'' then
     _result := false;
   {************************* widestring *************************}
{$ifdef haswidestring}
   { try to copy from an empty string }
   str3:='';
   str3:=Copy(str3,1,12);
   if str3<>'' then
     _result := false;

   str3:='';
   str3:=Copy('Hello world',0,12);
   if str3<>'Hello world' then
     _result := false;

   str3:='';
   str3:=Copy('Hello world',1,12);
   if str3<>'Hello world' then
     _result := false;

   str3:='';
   str3:=Copy('Hello world',-12,12);
   if str3<>'Hello world' then
     _result := false;

   str3:='';
   str3:=Copy('Hello world',64,128);
   if str3<>'' then
     _result := false;

   str3:='';
   str3:=Copy('Hello world',1,-12);
   if str3<>'' then
     _result := false;

   str3:='';
   str3:=Copy('Hello world',-12,0);
   if str3<>'' then
     _result := false;

   str3:='';
   str3:=Copy('Hello world',7,11);
   if str3<>'world' then
     _result := false;

   str3:='';
   str3:=Copy('Hello world',1,11);
   if str3<>'Hello world' then
     _result := false;

   str3:='';
   str3:=Copy('',0,12);
   if str3<>'' then
     _result := false;
{$endif}
    if not _result then
       fail
    else
      WriteLn('Success!');
  end;


procedure test_insert;
 var
   _result : boolean;
   i: integer;
 begin
   Write('Insert tests...');
   _result := true;
   {************************* shortstring ************************}
   str1:='Hello world';
   Insert(' this is my ',str1,-12);
   if str1<>' this is my Hello world' then
     _result := false;

   str1:='Hello world';
   Insert(' this is my ',str1,0);
   if str1<>' this is my Hello world' then
     _result := false;

   str1:='Hello world';
   Insert(' this is my ',str1,64);
   if str1<>'Hello world this is my ' then
     _result := false;

   str1:='Hello world';
   Insert(' this is my ',str1,300);
   if str1<>'Hello world this is my ' then
     _result := false;

   str1:='Hello world';
   Insert(' this is my ',str1,length(str1)+1);
   if str1<>'Hello world this is my ' then
     _result := false;

   str1:='Hello world';
   Insert('this is my ',str1,7);
   if str1<>'Hello this is my world' then
     _result := false;

   str1:='';
   Insert(' this is my ',str1,0);
   if str1<>' this is my ' then
     _result := false;

   str1:='';
   Insert(' this is my ',str1,length(str1));
   if str1<>' this is my ' then
     _result := false;

   str1:='';
   Insert(' this is my ',str1,32);
   if str1<>' this is my ' then
     _result := false;

   str1:='Hello world';
   Insert('',str1,0);
   if str1<>'Hello world' then
     _result := false;

   str1:='Hello world';
   Insert('',str1,7);
   if str1<>'Hello world' then
     _result := false;

   {************************* ansistring *************************}
   str2:='Hello world';
   Insert(' this is my ',str2,-12);
   if str2<>' this is my Hello world' then
     _result := false;

   str2:='Hello world';
   Insert(' this is my ',str2,0);
   if str2<>' this is my Hello world' then
     _result := false;

   str2:='Hello world';
   Insert(' this is my ',str2,64);
   if str2<>'Hello world this is my ' then
     _result := false;

   str2:='Hello world';
   Insert(' this is my ',str2,300);
   if str2<>'Hello world this is my ' then
     _result := false;

   str2:='Hello world';
   Insert(' this is my ',str2,length(str2)+1);
   if str2<>'Hello world this is my ' then
     _result := false;

   str2:='Hello world';
   Insert('this is my ',str2,7);
   if str2<>'Hello this is my world' then
     _result := false;

   str2:='';
   Insert(' this is my ',str2,0);
   if str2<>' this is my ' then
     _result := false;

   str2:='';
   Insert(' this is my ',str2,length(str2));
   if str2<>' this is my ' then
     _result := false;

   str2:='';
   Insert(' this is my ',str2,32);
   if str2<>' this is my ' then
     _result := false;

   str2:='Hello world';
   Insert('',str2,0);
   if str2<>'Hello world' then
     _result := false;

   str2:='Hello world';
   Insert('',str2,7);
   if str2<>'Hello world' then
     _result := false;

   {************************* widestring *************************}
{$ifdef haswidestring}
   str3:='Hello world';
   Insert(' this is my ',str3,-12);
   if str3<>' this is my Hello world' then
     _result := false;

   str3:='Hello world';
   Insert(' this is my ',str3,0);
   if str3<>' this is my Hello world' then
     _result := false;

   str3:='Hello world';
   Insert(' this is my ',str3,64);
   if str3<>'Hello world this is my ' then
     _result := false;

   str3:='Hello world';
   Insert(' this is my ',str3,300);
   if str3<>'Hello world this is my ' then
     _result := false;

   str3:='Hello world';
   Insert(' this is my ',str3,length(str3)+1);
   if str3<>'Hello world this is my ' then
     _result := false;

   str3:='Hello world';
   Insert('this is my ',str3,7);
   if str3<>'Hello this is my world' then
     _result := false;

   str3:='';
   Insert(' this is my ',str3,0);
   if str3<>' this is my ' then
     _result := false;

   str3:='';
   Insert(' this is my ',str3,length(str3));
   if str3<>' this is my ' then
     _result := false;

   str3:='';
   Insert(' this is my ',str3,32);
   if str3<>' this is my ' then
     _result := false;

   str3:='Hello world';
   Insert('',str3,0);
   if str3<>'Hello world' then
     _result := false;

   str3:='Hello world';
   Insert('',str3,7);
   if str3<>'Hello world' then
     _result := false;

{$endif}

   if not _result then
      fail
   else
     WriteLn('Success!');
 end;

 procedure test_pos;
  var
    _result : boolean;
    position: integer;
  begin
    Write('Pos tests...');
    _result := true;
   {************************* shortstring ************************}
   str1:='Hello world';
   position:=Pos('',str1);
   if position <> 0 then
     _result := false;

   str1:='';
   position:=Pos('',str1);
   if position <> 0 then
     _result := false;

   str1:='Hello world';
   position:=Pos('world',str1);
   if position <> 7 then
     _result := false;

   str1:='Hello world';
   position:=Pos('world',str1);
   if position <> 7 then
     _result := false;

   str1:='Hello world';
   position:=Pos('worldx',str1);
   if position <> 0 then
     _result := false;

   str1:='';
   position:=Pos('worldx',str1);
   if position <> 0 then
     _result := false;

   {************************* ansistring *************************}
   str2:='Hello world';
   position:=Pos('',str2);
   if position <> 0 then
     _result := false;

   str2:='';
   position:=Pos('',str2);
   if position <> 0 then
     _result := false;

   str2:='Hello world';
   position:=Pos('world',str2);
   if position <> 7 then
     _result := false;

   str2:='Hello world';
   position:=Pos('world',str2);
   if position <> 7 then
     _result := false;

   str2:='Hello world';
   position:=Pos('worldx',str2);
   if position <> 0 then
     _result := false;

   str2:='';
   position:=Pos('worldx',str2);
   if position <> 0 then
     _result := false;

   {************************* widestring *************************}
{$ifdef haswidestring}
   str3:='Hello world';
   position:=Pos('',str3);
   if position <> 0 then
     _result := false;

   str3:='';
   position:=Pos('',str3);
   if position <> 0 then
     _result := false;

   str3:='Hello world';
   position:=Pos('world',str3);
   if position <> 7 then
     _result := false;

   str3:='Hello world';
   position:=Pos('world',str3);
   if position <> 7 then
     _result := false;

   str3:='Hello world';
   position:=Pos('worldx',str3);
   if position <> 0 then
     _result := false;

   str3:='';
   position:=Pos('worldx',str3);
   if position <> 0 then
     _result := false;

{$endif}
    if not _result then
       fail
    else
      WriteLn('Success!');
  end;

 procedure test_chr;
  var
   c: char;
   _result : boolean;
  begin
    Write('Chr tests...');
    _result := true;
{    c:=chr($3074);
     if c<>'t' then
       _result := false;
  The above statement compile under Delphi, and it
  should not imho. Freepascal gives a range-check
  error, as it should.
}
    if chr(76)<>'L' then
      _result := false;

    if _result = false then
      fail
    else
      WriteLn('Success!');
  end;


 procedure test_concat;
 var
   _result : boolean;
   i: integer;
 begin
   Write('Concat tests...');
   _result := true;
   if not _result then
      fail
   else
     WriteLn('Success!');
 end;

Begin
  test_delete;
  test_stringofchar;
  test_copy;
  test_insert;
  test_pos;
  test_chr;
end.
