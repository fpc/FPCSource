unit PasPrep;
interface
uses
 Comments;
const
 PasNesting:longbool=true;
procedure do_pascal(__buf:pointer;size:longint;proc:pointer);
implementation
type
 at=array[1..1]of char;
 pat=^at;
 str255=string[255];
procedure do_pascal(__buf:pointer;size:longint;proc:pointer);
var
 old,i:longint;
 buf:pat absolute __buf;
const
 GetWord_Pos:longint=0;
 LastWord:str255='';
 StringBody:longbool=false;
procedure GetWord;
 begin
  LastWord:='';
  if GetWord_Pos>size then
   exit;
  while buf^[GetWord_Pos]<=#32 do
   begin
    if GetWord_Pos>size then
     exit;
    inc(GetWord_Pos);
   end;
  repeat
   if buf^[GetWord_Pos]=''''then
    StringBody:=not StringBody;
   LastWord:=LastWord+upcase(buf^[GetWord_Pos]);
   inc(GetWord_Pos);
   if GetWord_Pos>size then
    break;
   if(buf^[GetWord_Pos]in[#0..#32,';'])and not StringBody then
    break;
  until false;
  while(length(LastWord)>1)and(lastWord[1]=';')do
   begin
    inc(GetWord_Pos);
    delete(LastWord,1,1);
   end;
 end;
function IsTypeDef(pos:longint):longbool;
 var
  i:longint;
 begin
  IsTypeDef:=false;
  for i:=pos downto 1 do
   if buf^[i]>=#32 then
    begin
     IsTypeDef:=buf^[i]in['=',':'];
     exit;
    end;
 end;
procedure JumpToNext;
var iLastword: Longint;
begin
  repeat
   iLastword:=GetWord_Pos;
   if GetWord_Pos>size then
    exit;
   GetWord;
   i:=GetWord_Pos;
   if(LastWord='EXTERNAL')or(LastWord='FORWARD')or(LastWord='INLINE')then
    break
   else if (LastWord='CONST')then begin
          GetWord_Pos:=iLastword;
          break;
        end;
  until false;
end;

procedure JumpToEnd;
 var
  mainBegin:str255;
 procedure do_body;
  var
   level:longint;
  begin
   level:=1;
   while level>0 do
    begin
     if GetWord_Pos>size then
      exit;
     GetWord;
     if (LastWord='BEGIN')or(LastWord='ASM')or(LastWord='CASE')then
      inc(level)
     else if (LastWord='END')then
      dec(level);
    end;
  end;
 begin
  mainBegin:='BEGIN';
  repeat
   if GetWord_Pos>size then
    exit;
   GetWord;
   i:=GetWord_Pos;
   if((LastWord='PROCEDURE')or(lastword='FUNCTION')or(lastword='OPERATOR'))and not isTypedef(old)then
    JumpToEnd
   else if(LastWord='EXTERNAL')or(LastWord='FORWARD')or(LastWord='INLINE')then
    exit
   else if (LastWord='ASSEMBLER')then
    mainBegin:='ASM';
  until LastWord=mainBegin;
  do_body;
 end;
procedure do_consts(savefunc:pointer);
 type
  Tpushfunc=procedure(const key,value:str255;CaseSent:longbool);
 var
  old,k,kk:longint;
  s:str255;
  ss:array[1..2]of str255;
  pushfunc:Tpushfunc absolute SaveFunc;
 begin
  repeat
   if GetWord_Pos>size then
    exit;
   old:=GetWord_Pos;
   GetWord;
   if(((LastWord='PROCEDURE')or(lastword='FUNCTION')or(lastword='OPERATOR'))and not isTypedef(old))
     or(lastword='TYPE')
     or(lastword='CONST')
     or(lastword='VAR')then
    begin
     GetWord_Pos:=old;
     exit;
    end
   else
    begin
     s:=LastWord;
     while LastWord<>';'do
      begin
       GetWord;
       if GetWord_Pos>size then
        exit;
       s:=s+LastWord;
      end;
     if s[length(s)]=';'then
      dec(s[0]);
     if s<>''then
      if pos(':',s)=0 then
       if pos('=',s)>0 then
        begin
         ss[1]:='';
         ss[2]:='';
         kk:=1;
         for k:=1 to length(s)do
          begin
           if s[k]>#32 then
            begin
             if(s[k]='=')and(kk=1)then
              inc(kk)
             else
              ss[kk]:=ss[kk]+s[k];
            end;
          end;
         TpushFunc(PushFunc)(ss[1],ss[2],false);
        end;
    end;
  until false;
 end;
begin
 ClearComments(PasNesting,buf,size);
 i:=1;
 GetWord_Pos:=0;
 while i<=size do
  begin
   old:=GetWord_Pos;
   GetWord;
   i:=GetWord_Pos;
   if (lastword='OPERATOR')and not isTypedef(old)then
    JumpToEnd
   else if ((LastWord='PROCEDURE')or(lastword='FUNCTION')) and not isTypedef(old) then
    JumpToNext
   else if LastWord='CONST'then
    Do_Consts(proc)
   else if LastWord='IMPLEMENTATION'then
    exit;
  end;
end;
end.
