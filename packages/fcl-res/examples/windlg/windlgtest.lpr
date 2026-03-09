program windlgtest;

{$mode objfpc}{$H+}

{ Link the resource file compiled by fpcres }
{$R testdlg.res}

uses
  Windows;

const
  IDD_DIALOG   = 100;
  IDD_DIALOGEX = 200;

{ Dialog procedure for both dialogs }
function DlgProc(hDlg: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): INT_PTR; stdcall;
begin
  Result := INT_PTR(False);
  case uMsg of
    WM_INITDIALOG:
      begin
        Result := INT_PTR(True);
      end;
    WM_COMMAND:
      begin
        case LOWORD(wParam) of
          IDOK:
            begin
              EndDialog(hDlg, IDOK);
              Result := INT_PTR(True);
            end;
          IDCANCEL:
            begin
              EndDialog(hDlg, IDCANCEL);
              Result := INT_PTR(True);
            end;
        end;
      end;
    WM_CLOSE:
      begin
        EndDialog(hDlg, IDCANCEL);
        Result := INT_PTR(True);
      end;
  end;
end;

var
  Res: INT_PTR;
begin
  WriteLn('Showing DIALOG (resource 100)...');
  Res := DialogBox(HInstance, MAKEINTRESOURCE(IDD_DIALOG), 0, @DlgProc);
  if Res = -1 then
    WriteLn('  DialogBox failed, GetLastError=', GetLastError)
  else
    WriteLn('  Result: ', Res, ' (1=OK, 2=Cancel)');

  WriteLn;

  WriteLn('Showing DIALOGEX (resource 200)...');
  Res := DialogBox(HInstance, MAKEINTRESOURCE(IDD_DIALOGEX), 0, @DlgProc);
  if Res = -1 then
    WriteLn('  DialogBox failed, GetLastError=', GetLastError)
  else
    WriteLn('  Result: ', Res, ' (1=OK, 2=Cancel)');

  WriteLn;
  WriteLn('Done. Press enter to exit');
  Readln;
end.
