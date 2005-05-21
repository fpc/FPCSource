program smallplay;

{****************************************

 PT-Player 2.0   © 1994 BetaSoft

 uses PTReplay.library also by BetaSoft
 and Andreas [Pucko] Pålsson

****************************************}
uses exec, amigados, ptreplay;

const
    vstr : pchar = '$VER: SmallPlay 2.0 (23.12.93)';

var
    module : pModule;
    SigBit : shortint;
    SigMask : longint;

procedure CleanUp(why : string, err : integer);
begin
    if why <> '' then writeln(why);
    halt(err);
end;

begin
    module := nil;
    if ParamCount > 1 then
       CleanUp('Specify one module only',20);
    if ParamCount < 0 then
       CleanUp('Play what module?',20);

    module := PTLoadModule(ParamStr[1]);
    if not assigned(module) then
       CleanUp('Couldn''t open/load module',20);

    SigBit := AllocSignal(-1);
    if SigBit = -1 then
       CleanUp('Couldn''t allocate signal',10);

    PTInstallBits(module,SigBit,-1,-1,-1);
    PTPlay(module);

    SigMask := Wait(SIGBREAKF_CTRL_C or (1 shl SigBit));
    if (SigMask and SIGBREAKF_CTRL_C) then
        PTFade(module,1)
    else
        PTStop(module);

    FreeSignal(SigBit);

    PTUnloadModule(module);
end.
