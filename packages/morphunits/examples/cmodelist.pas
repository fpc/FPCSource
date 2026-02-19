{
    Copyright (c) 2026 Karoly Balogh

    CyberGraphX API, mode list query example program
    Example program for Free Pascal's MorphOS bindings

    This example program is in the Public Domain under the terms of
    Unlicense: http://unlicense.org/

 **********************************************************************}

program cmodelist;

uses
  exec, utility, cybergraphics;

var
  bestmode: DWord;
  modelist: PList;
  mode: PCyberModeNode;

begin
  bestmode:=BestCModeIDTags([
          CYBRBIDTG_NominalWidth, 640,
          CYBRBIDTG_NominalHeight, 480,
          CYBRBIDTG_Depth, 24,
          TAG_END
      ]);
  writeln('Best Mode ID: $',hexstr(bestmode,8));

  modelist:=AllocCModeListTags([
          CYBRMREQ_MinWidth, 640,
          CYBRMREQ_MinHeight, 480,
          CYBRMREQ_MinDepth, 15,
          TAG_END
      ]);
  if assigned(modelist) then
    begin
      mode:=PCyberModeNode(modelist^.lh_Head);
      while PNode(mode) <> @modelist^.lh_Tail do
        begin
          write('ID: $',hexstr(mode^.DisplayID,8),'  Mode: ',mode^.ModeText);
          if mode^.DisplayID = bestmode then
            write(' <-- Best Mode ID');
          writeln;
          mode:=PCyberModeNode(mode^.Node.ln_Succ);
        end;
      FreeCModeList(modelist);
    end;
end.
