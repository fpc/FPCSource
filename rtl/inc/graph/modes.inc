{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,99 by the Free Pascal development team

    This include implements video mode management.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


 procedure addmode(mode: TModeInfo);
  {********************************************************}
  { Procedure AddMode()                                    }
  {--------------------------------------------------------}
  { This routine adds <mode> to the list of recognized     }
  { modes. Duplicates are allowed.                         }
  {********************************************************}
  var
    list: PModeInfo;
    newlst : PModeInfo;
  begin
    if not assigned(ModeList) then
      begin
        new(ModeList);
        move(mode, ModeList^, sizeof(TModeInfo));
      end
    else
      begin
        list := ModeList;
        { go to the end of the list }
        while assigned(list^.next) do
          list:=list^.next;
        new(NewLst);
        list^.next := NewLst;
        move(mode, NewLst^, sizeof(TModeInfo));
      end;
  end;


  procedure initmode(var mode: TModeInfo);
  {********************************************************}
  { Procedure InitMode()                                   }
  {--------------------------------------------------------}
  { This routine initialized the mode to default values.   }
  {********************************************************}
   begin
     FillChar(mode,sizeof(TModeInfo),#0);
   end;


   function searchmode(ReqDriver : integer; reqmode: integer): PModeInfo;
  {********************************************************}
  { Procedure SearchMode()                                 }
  {--------------------------------------------------------}
  { This routine searches the list of recognized modes,    }
  { and tries to find the <reqmode> in the <reqdriver>     }
  { return nil if not found, otherwise returns the found   }
  { structure.                                             }
  {********************************************************}
    var
     list: PModeInfo;
    begin
        searchmode := nil;
        list := ModeList;
        { go to the end of the list }
        while assigned(list) do
          begin
             if (list^.DriverNumber = ReqDriver) and
               (list^.ModeNumber = ReqMode) then
               begin
                 searchmode := list;
                 exit;
               end;
             list:=list^.next;
          end;
    end;


   procedure cleanmode;far;
  {********************************************************}
  { Procedure CleanMode()                                  }
  {--------------------------------------------------------}
  { This routine deallocates the mode list.                }
  { It is called as an exit procedure ONLY.                }
  {********************************************************}
    var
      list: PModeInfo;
      tmp : PModeInfo;
    begin
        list := ModeList;
        { go to the end of the list }
        while assigned(list) do
        begin
          tmp := list;
          list:=list^.next;
          dispose(tmp);
        end;
    end;


