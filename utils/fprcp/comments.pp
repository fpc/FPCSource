unit Comments;
interface
procedure ClearComments(nesting:longbool;__buf:pointer;size:longint);
implementation
procedure ClearComments(nesting:longbool;__buf:pointer;size:longint);
 type
  tat=array[1..1]of char;
  pat=^tat;
  pblock=^tblock;
  tblock=record
   next:pblock;
   _begin,_end:longint;
  end;
 type
  str255=string[255];
 var
  CommLevel:longint;
  buf:pat absolute __buf;
  i,j:longint;
  comm:pblock;
 function TwoChars(const s):str255;
  var
   d:tat absolute s;
   ii:longint;
  begin
   TwoChars:='  ';
   if succ(i)>=size then
    TwoChars:=''
   else
    begin
     ii:=2;
     TwoChars[1]:=d[1];
     TwoChars[ii]:=d[ii];
    end;
  end;
 function FindFrom(position:longint;const Origin:str255):longint;
  var
   j,k:longint;
  begin
   FindFrom:=size;
   for j:=position to Size-length(Origin)do
    begin
     for k:=1 to length(Origin)do
      begin
       if buf^[j+k-1]<>Origin[k]then
        break
       else if k=length(Origin)then
        begin
         FindFrom:=j;
         exit;
        end;
      end;
    end;
  end;
 procedure BeginComment(i:longint);
  var
   c:pBlock;
  begin
   new(c);
   c^.next:=comm;
   c^._begin:=i;
   c^._end:=size;
   comm:=c;
   CommLevel:=1;
  end;
 procedure EndComment(i:longint);
  begin
   if comm<>nil then
    comm^._end:=i;
   dec(CommLevel);
  end;
 procedure DeleteComments;
  var
   i:longint;
   c,cc:pblock;
  begin
   c:=comm;
   while c<>nil do
    begin
     for i:=c^._begin to c^._end do
      buf^[i]:=#32;
     cc:=c;
     c:=c^.next;
     dispose(cc);
    end;
  end;
 begin
  commLevel:=0;
  comm:=nil;
  i:=1;
  while i<size do
   begin
    if commlevel=0 then
     begin
      if buf^[i]=''''then
       i:=FindFrom(succ(i),'''');
      if TwoChars(buf^[i])='//'then
       begin
        BeginComment(i);
        j:=FindFrom(succ(i),#13);
        if j=size then
         j:=FindFrom(succ(i),'#10');
        i:=j;
        EndComment(i);
       end;
      if(buf^[i]='{')or(TwoChars(buf^[i])='(*')then
       BeginComment(i);
     end
    else
     begin
      if(buf^[i]='{')or(TwoChars(buf^[i])='(*')then
       begin
        if nesting then
         inc(CommLevel);
       end;
      if(buf^[i]='}')or(TwoChars(buf^[i])='*)')then
       EndComment(succ(i));
     end;
    inc(i);
   end;
  DeleteComments;
 end;
end.
