program muihelloworld;

// Example Source for MUIHelper, Simple Window and Button

uses
  Exec, Utility, intuition, AmigaDos, mui, muihelper;

procedure StartMe;
var
  App, Window, Button, NLabel: PObject_;
  Sigs: LongInt;
begin
  App := MH_Application([
    MUIA_Application_Title,       AsTag('MUIHelloWorld'),
    MUIA_Application_Version,     AsTag('$VER: MUIHelloWorld 1.0 (18.12.2016)'),
    MUIA_Application_Copyright,   AsTag('©2016, FreePascal Developer'),
    MUIA_Application_Author,      AsTag('FreePascal Developer'),
    MUIA_Application_Description, AsTag('Hello World Demo for MUI Helper'),
    MUIA_Application_Base,        AsTag('HELLOWORLD'),

    SubWindow, AsTag(MH_Window(Window, [
      MUIA_Window_Title,  AsTag('Hello World'),
      MUIA_Window_ID,     MAKE_ID('H','E','W','O'),

      WindowContents, AsTag(MH_VGroup([
        Child, AsTag(MH_CLabel(NLabel, 'Hello')),
        Child, AsTag(MH_CLabel(NLabel, 'World')),
        Child, AsTag(MH_Button(Button, 'Click me')),
      TAG_END])), // MH_VGroup()
    TAG_END])), // MH_Window()
  TAG_END]); // MH_Application()
  //
  // Check if successfully Created
  if not Assigned(App) then
  begin
    WriteLn('Failed to create Application');
    Exit;
  end;
  // Connect Close Event
  DoMethod(Window, [MUIM_Notify, MUIA_Window_CloseRequest, MUI_TRUE,
    AsTag(App), 2, AsTag(MUIM_Application_ReturnID), AsTag(MUIV_Application_ReturnID_Quit)]);

  // Change Label on Button Click
  DoMethod(Button, [MUIM_Notify, MUIA_Pressed, MUI_FALSE,
    AsTag(NLabel), 3, AsTag(MUIM_SET), AsTag(MUIA_Text_Contents), AsTag('FreePascal')]);

  // Open the Window
  MH_Set(Window, MUIA_Window_Open, AsTag(True));

  // Main Loop
  if MH_Get(Window, MUIA_Window_Open) <> 0 then
  begin
    while Integer(DoMethod(app, [MUIM_Application_NewInput, AsTag(@sigs)])) <> MUIV_Application_ReturnID_Quit do
    begin
      if Sigs <> 0 then
      begin
        Sigs := Wait(sigs or SIGBREAKF_CTRL_C);
        if (Sigs and SIGBREAKF_CTRL_C) <>0 then
          Break;
      end;
    end;
  end;
  // Close Window
  MH_Set(Window, MUIA_Window_Open, AsTag(True));
  // Free MUI Objects
  MUI_DisposeObject(app);
end;

begin
  StartMe;
end.
