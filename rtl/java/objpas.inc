{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe
    member of the Free Pascal development team.

    This file implements the helper routines for TObject

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

  procedure TObject.Free;
    begin
      if not DestructorCalled then
        begin
          DestructorCalled:=true;
          Destroy;
        end;
    end;


  destructor TObject.Destroy;
    begin
    end;


  procedure TObject.Finalize;
    begin
      Free;
    end;


