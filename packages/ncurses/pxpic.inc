{---------------------------------------------------------------------------
                                 CncWare
              Created and Copyright (c) 1991  J. John Sprenger
----------------------------------------------------------------------------
  Filename..: pxpic.inc
  Programmer: Ken J. Wright, ken@cncware.com
  Date......: 06/09/2000

  Purpose - Duplicates the functionality of the TPXPictureValidator.IsValid
            method from Turbo Vision's validate unit. This function was
            extracted from a unit called fmtline written by J. John Sprenger.
            It was actually written before the validate unit was available
            from Borland in TV2.0.

-------------------------------<< REVISIONS >>--------------------------------
  Ver  |   Date   | Prog| Description
-------+----------+-----+-----------------------------------------------------
  1.00 | 06/10/00 | kjw | Initial Release.
  1.01 | 06/11/00 | kjw | Finally debugged the spin cycle! The AnyLeft function
                        | missed a condition that left it an endless loop.
                        | Added the boolean "done" to fix it.
  1.02 | 06/15/00 | kjw | Added '@' to the match set.
------------------------------------------------------------------------------}

  {    Created and Copyright (c) 1991  J. John Sprenger    }

  { tFormatLine.CheckPicture is the function that inspects }
  { the input string passed as S against the Pic string    }
  { which holds the Paradox-form Picture.  If an error is  }
  { found the position of the error is placed in CPos.     }

function nCheckPxPicture(var s, Pic : string;
                      var CPos : integer) : word;
  const
    { flError, flCharOk and flFormatOK are constants used  }
    { by tFormatLine.CheckPicture.  flError is returned    }
    { when an error is found,  flCharOk when an character  }
    { is found to be appropriate,  And flFormatOk when the }
    { entire input string is found acceptable.             }
    flError    = $0000;
    flCharOK   = $0001;
    flFormatOK = $0002;

  var
    Resolved  : integer;
    TempIndex : integer;

  { Function Copy represents a bit of syntactic sugar for  }
  { the benefit of the author.  It changes the Copy func.  }
  { so that its parameters represent start and end points  }
  { rather than a start point followed by a quantity.      }
  function Copy(s : string; start, stop : integer) : string;
  begin
    if stop < start then Copy:=''
    else Copy:=System.Copy(s,start,stop-start+1);
  end;

  { Function FindMatch recursively locates the matching   }
  (* grouping characters for "{" and "[".                *)
  function FindMatch(P : string) : integer;
  var
    i:integer;
    match:boolean;
  begin
    i:=2;
    match:=false;
    while (i<=length(P)) and not match do begin
      if ((p[i]=']') and (p[1]='[')) or ((p[i]='}') and
        (p[1]='{')) then
        match:=true;
      if p[i]='{' then
        i:=i+FindMatch(Copy(p,i,length(p)))
      else
        if p[i]='[' then
          i:=i+FindMatch(Copy(p,i,length(P)))
        else inc(i);
    end;
    FindMatch:=i-1;
  end;

  { Function CP is the heart of tFormatLine.  It           }
  { determines if the string, s, passed to it fits the     }
  { requirements of the picture, Pic.  The number of       }
  { characters successfully resolved is returned in the    }
  { parameter resolved. When groups or repetitions are     }
  { encountered CP will call itself recursively.           }
  function CP(var s : string; Pic : string; var CPos :
              integer; var Resolved : integer) : word;
  const
     CharMatchSet = ['#', '?', '&', '''', '@', '!'];
  var
    i          : integer;
    index      : integer;
    result_     : word;
    commit     : boolean;
    Groupcount : integer;

  { Procedure Succeed resolves defaults and <Space>        }
  { default requests                                       }

  { Note:
    The little patch below to exclude group end checking during
    expansion lets autofill work as it should, however it also
    autofills prematurely when there are more optionals or
    alternates. I haven't quite figured how to make this work
    correctly within the current recursion scheme.
    kjw
  }
    procedure Succeed;
    var
      t     : integer;
      found : boolean;
    begin
      if (i <= Length(s)) and
         (s[i]=' ') and
         (Pic[index]<>' ') and
         (Pic[index]<>',')
      then begin
        t:=index;
        found:=false;
        while (t<=length(pic)) and not found do begin
          if not (Pic[t] in (CharMatchSet+
                 ['*','[','{',',',']','}'])) then begin
            if pic[t]=';' then inc(t);
            s[i]:=Pic[t];
            found:=true;
          end;
          inc(t);
        end;
      end;
      if (i>length(s)) then
        {----------------------}
        { Expand with defaults }
        while not (Pic[index] in
              (CharMatchSet+['*','[','{',',',']','}'])) and
              (index<=length(Pic)) and
              not(Pic[index-1] in [(*'}',*)','(*,']'*)]) do begin {kjw}
          if Pic[index]=';' then inc(index);
          s[i]:=Pic[index];
          if i>length(s) then begin
            CPos:=i;
            s[0]:=char(i);
          end;
          inc(i);
          inc(index);
        end;
    end;

  { Function AnyLeft returns true if there are no required }
  { characters left in the Picture string.                 }
    function AnyLeft : boolean;
    var
       TempIndex : integer;
       done : boolean; {kjw, 06/11/2000}
    begin
      done := false;
      TempIndex:=index;
      while ((Pic[TempIndex]='[') or (Pic[TempIndex]='*'))
            and (TempIndex<=Length(Pic))
            and (Pic[TempIndex]<>',')
            and not done do begin
        if Pic[TempIndex]='[' then
          Tempindex:=Tempindex+FindMatch(Copy(Pic,index, Length(Pic)))
        else begin
          if not (Pic[TempIndex+1] in ['0'..'9']) then begin
            inc(TempIndex);
            if Pic[TempIndex] in ['{','['] then
              tempIndex:=TempIndex+ FindMatch(Copy(pic,index,length(pic)))
            else inc(TempIndex);
          end else done := true;
        end;
      end;
      AnyLeft:=(TempIndex<=length(Pic)) and
               (Pic[TempIndex]<>',');
    end;

  { Function CharMatch determines if the current character }
  { matches the corresponding character mask in the        }
  { Picture string. Alters the character if necessary.     }
    function CharMatch : word;
    var result_ : word;
    begin
      result_:=flError;
      case Pic[index] of
        '#': if s[i] in ['0'..'9'] then result_:=flCharOk;
        '?': if s[i] in ['A'..'Z','a'..'z'] then
          result_:=flCharOk;
        '&': if s[i] in ['A'..'Z','a'..'z'] then
          begin
            result_:=flCharOk;
            s[i]:=upcase(s[i]);
          end;
       '''': result_:=flCharOk;
        '@': result_:=flCharOk;
        '!': begin
           result_:=flCharOk;
           s[i]:=upcase(s[i]);
        end;
      end;
      if result_<>flError then commit:=true;
      CharMatch:=result_;
    end;

  { Function Literal handles characters which are needed    }
  { by the picture but otherwise used as format specifiers. }
  { All such characters are preceded by the ';' in the      }
  { picture string.                                         }
    function Literal : word;
    var result_ : word;
    begin
      inc(index);
      if s[i]=Pic[index] then result_:=flCharOk
      else result_:=flError;
      if result_<>flError then commit:=true;
      Literal:=result_;
    end;

  { Function Group handles required and optional groups    }
  { in the picture string.  These are designated by the    }
  (* "{","}" and "[","]" character pairs.                 *)
    function Group:word;
    var
      result_: word;
      TempS: string;
      TempPic: string;
      TempCPos: integer;
      PicEnd: integer;
      TempIndex: integer;
      SwapIndex:integer;
      SwapPic : string;
    begin
      TempPic:=Copy(Pic,index,length(Pic));
      PicEnd:=FindMatch(TempPic);
      TempPic:=Copy(TempPic,2,PicEnd-1);
      TempS:=Copy(s,i,length(s));
      TempCPos:=1;

      result_:=CP(TempS,TempPic,TempCPos,TempIndex);

      if result_=flCharOK then inc(GroupCount);
      if (result_=flFormatOK) and (groupcount>0) then
        dec(GroupCount);
      if result_<>flError then result_:=flCharOk;

      SwapIndex:=index;
      index:=TempIndex;
      SwapPic:=Pic;
      Pic:=TempPic;
      if not AnyLeft then result_:=flCharOk;
      pic:=SwapPic;
      index:=SwapIndex;
      if i>1 then s:=copy(s,1,i-1)+TempS else s:=TempS;

      CPos:=Cpos+TempCPos-1;
      if Pic[index]='[' then begin
        if result_<>flError then
           i:=i+TempCPos-1
        else dec(i);
        result_:=flCharOK;
      end
      else i:=i+TempCPos-1;
      index:=index+PicEnd-1;
      Group:=result_;
    end;

  { Function Repetition handles characters that may be     }
  { repeated in the input string.  The picture string      }
  { indicates this possiblity with "*" character.          }
    function Repetition:word;
    var
      result_:word;
      count:integer;
      TempPic:string;
      TempS:string;
      TempCPos:integer;
      TempIndex:integer;
      SwapIndex:integer;
      SwapPic:string;
      PicEnd:integer;
      commit:boolean;

      procedure MakeCount;
      var nstr:string;
          code:integer;
      begin
        if Pic[index] in ['0'..'9'] then begin
          nstr:='';
          repeat
            nstr:=nstr+Pic[index];
            inc(index);
          until not(Pic[index] in ['0'..'9']);
          val(nstr,count,code);
        end
        else count:=512;
      end;

      procedure MakePic;
      begin
        if Pic[index] in ['{','['] then begin
          TempPic:=copy(Pic,index,length(Pic));
          PicEnd:=FindMatch(TempPic);
          TempPic:=Copy(TempPic,2,PicEnd-1);
        end
      else begin
        if Pic[index]<>';' then begin
          TempPic:=''+Pic[index];
          PicEnd:=3;
          if index=1 then
            pic:='{'+pic[index]+'}'+ copy(pic,index+1,length(pic))
          else pic:=copy(pic,1,index-1)+
                         '{'+pic[index]+'}'+
                         copy(pic,index+1,length(pic));
        end
        else begin
          TempPic:=Pic[index]+Pic[index+1];
          PicEnd:=4;
          if index=1 then
            pic:='{' + pic[index] + pic[index+1]+'}' +
                 copy(pic,index+1,length(pic))
          else pic:=copy(pic,1,index-1) + '{' + pic[index] +
                    pic[index+1] + '}' + copy(pic,index+1,length(pic));
        end;
      end;
    end;

    begin
      inc(index);
      MakeCount;
      MakePic;
      result_:=flCharOk;
      while (count<>0) and (result_<>flError) and
            (i<=length(s)) do begin
        commit:=false;
        TempS:=Copy(s,i,length(s));
        TempCPos:=1;
        result_:=CP(TempS,TempPic,TempCPos,TempIndex);

        if result_=flCharOK then inc(GroupCount);
        if (result_=flFormatOK) and (groupcount > 0) then
          dec(GroupCount);
        if result_<>flError then result_:=flCharOk;

        SwapIndex:=Index;
        Index:=TempIndex;
        SwapPic:=Pic;
        Pic:=TempPic;
        if (not AnyLeft) then result_:=flCharOk;
        Pic:=SwapPic;
        index:=SwapIndex;
        if i>1 then s:=copy(s,1,i-1)+TempS else s:=TempS;
        Cpos:=Cpos+TempCpos-1;
        if (count>255) then begin
          if result_<>flError then begin
            i:=i+TempCpos-1;
            if not commit then commit:=true;
            result_:=flCharOk;
          end
          else dec(i);
        end
          else i:=i+TempCPos-1;
        inc(i);
        dec(count);
      end;
      dec(i);
      index:=index+PicEnd-1;
      if result_=flError then
         if (count>255) and not commit
           then result_:=flCharOk;
      repetition:=result_;
    end;

    begin { of function CP}
      i:=1;
      index:=1;
      result_:=flCharOk;
      commit:=false;
      Groupcount:=0;
      while (i<=length(s)) and (result_<>flError) do begin
        if index>length(Pic) then result_:=flError
        else begin
          if s[i]=' ' then Succeed;
          if Pic[index] in CharMatchSet then
            result_:=CharMatch
          else
            if Pic[index]=';' then
              result_:=Literal
            else
              if (Pic[index]='{') or (Pic[index]='[') then
                result_:=Group
              else
                if Pic[index]='*' then
                  result_:=Repetition
                else
                  if Pic[index] in [',','}',']'] then
                    result_:=flError
                  else
                    if Pic[index]=s[i] then begin
                      result_:=flCharOk;
                      commit:=true;
                    end
                    else result_:=flError;
          if (result_ = flError) and not commit then begin
            TempIndex:=Index;
            while (TempIndex<=length(Pic)) and
                  ((Pic[TempIndex]<>',') and
                  (Pic[TempIndex-1]<>';'))  do begin
              if (Pic[TempIndex]='{') or
                 (Pic[TempIndex]=']') then
                Index:=FindMatch(Copy( Pic,
                                 TempIndex,length(Pic)))+TempIndex-1;
                inc(TempIndex);
            end;
            if Pic[TempIndex]=',' then begin
              if Pic[TempIndex-1]<>';' then begin
                result_:=flCharOk;
                index:=TempIndex;
                inc(index);
              end;
            end;
          end
          else if result_<>flError then begin
            inc(i);
            inc(index);
            Succeed;
          end;

        end;
      end;
      Resolved:=index;

      if (result_=flCharOk) and
         (GroupCount=0) and
         (not AnyLeft or ((Pic[index-1]=',') and
         (Pic[index-2]<>';'))) then
         result_:=flFormatOk;

      CPos:=i-1;
      CP:=result_;
    end;

begin{ of function CheckPicture}
   Resolved:=0;
   CPos := 0;
   If (Pic = '') or (s = '') Then
      nCheckPxPicture := flFormatOk
   Else
      nCheckPxPicture:=CP(s,Pic,CPos,Resolved);
end;
