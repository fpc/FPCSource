{ Old file: tbs0336.pp }
{  }

{$mode objfpc}
Uses classes,sysutils;


const dsmerged=0;
      dsopenerror=1;
      dscreateerror=2;
      dsconverterror=3;
      dsmismatcherror=4;
      dscrcerror=5;
      dserror=6;

type tvsmergediffs=class
                     procedure execute;
                     end;

     tvsdiffitem= class
                                 status : longint;
                    end;

EMismatchedDiffError =class(exception);
EDiffCrcCompareError= class(exception);

procedure TvsMergeDiffs.Execute;
var
  Stream: tFileStream;
  Item: TvsDiffItem;
  a : longint;
begin
      try
        Item.Status := dsMerged;
      except
        { Only the number of on xx do statements seems to matter, not
                which ones, try commenting 3 or 4 out}
        on EFOpenError do Item.Status := dsOpenError;
        on EFCreateError do Item.Status := dsCreateError;
        on EConvertError do Item.Status := dsConvertError;
        on EMismatchedDiffError do Item.Status := dsMismatchError;
        on EDiffCrcCompareError do Item.Status := dsCrcError;
        on Exception do Item.Status := dsError;
      end;
end;

begin
end.
