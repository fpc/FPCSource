{* tcl_demo.pas
 * ------------------------------------------------
 * Copyright 2002 by Bert Raccoon aka Max Artemev
 * (bert@furry.ru, bert_raccoon@freemail.ru)
 * ------------------------------------------------
 * Demo for tcl80.pas unit.
 * Creating the Tcl interpreter, executing file, registering
 * new commands, call a function defined in the script.
 *
 * Under Win32 can cause crash.
 *}
program test;

uses tcl80, SysUtils;

{*
 * Function for testing that string is correct number
 *}
function is_num(const src: PChar): Boolean;
var
   i: integer;
begin
     is_num:=True;
     i:=0;
     repeat
        is_num:=is_num and ((src + i)^  in ['0'..'9']);
        inc(i);
     until (src + i)^ = #0;
end;

function Test_max(clientData: Tcl_ClientData;  {* Some user defined data. *}
                  interp: PTcl_Interp;         {* Pointer to Tcl interpreter *}
                  argc: integer;               {* Arguments counter, arguments, etc *}
                  argv: Tcl_Argv): longint;    {* Remeber! *NIX `integer` type is 16 bit! *}
                  cdecl;                       {* C calling convention *}
var
   arg    : PChar;
   idx,
   value,
   maxVal : LongInt;
begin
     maxVal := 0; {* Zero variable. Very stupid comment? ;)) *}

     {* The `max` can be done with at least two digits
      * In ArgvItem(argv,0) passed function name
      *}
     if (argc < 3) then
     begin
          Tcl_AppendResult(interp, ['bad # arg: ', ArgvItem(argv,0), ' num1 num2 [..numN]', nil]);
          {* Under Win32 calling of this function can cause crash *}

          Test_max:=TCL_ERROR;         {* Error was occured *}
          exit;                        {* Leave *}
     end;

     for idx := 1 to argc-1 do         {* In argv[0] passed function name, so
                                        * go from the first index, not zero.
                                        *}
     begin
          arg := ArgvItem(argv,idx);   {* get an argument *}
          if (not is_num(arg)) then    {* Is right number? *}
          begin
               Tcl_AppendResult(interp,[' "', arg, '" is not a valid integer value']);
               Test_max:=TCL_ERROR;    {* Error was occured *}
               exit;                   {* leave *}
          end;
          Value:=StrToInt(arg);        {* Convert PChar->Integer *}
          if (value > maxVal) then
             maxVal := value;          {* Calculate maximum number *}
     end;

     {* Set the result for the our function.
      * result type always is PChar
      *}
     Tcl_SetResult(interp, PChar(IntToStr(maxVal)), nil);

     {* exit successful *}
     Test_max:=TCL_OK;
end;

{*
 *  Old and good known Pascal procedure :)
 *}
function Test_writeln(clientData: Tcl_ClientData; interp: pTcl_Interp; argc: integer; argv: Tcl_Argv): longint; cdecl;
var
   i: integer;
   Buff: string;
begin
     Buff := '';
     for i:=1 to argc-1 do
       Buff:=Buff + ArgvItem(argv,i);  {* work around some bugs *}
     writeln(Buff);
     Test_writeln:=TCL_OK;
end;


var
   interp: PTcl_Interp;
   code: integer;
begin
     interp := Tcl_CreateInterp();  {* Create an interpreter *}
     Tcl_Init(interp);              {* Initialize *}

     {* Register/override in the Tcl engine our new functions *}
     Tcl_CreateCommand(interp,'max', TTclCmdProc(@Test_max),nil,nil);
     Tcl_CreateCommand(interp,'writeln',TTclCmdProc(@Test_writeln),nil,nil);

     code := Tcl_EvalFile(interp,'test.tcl');   {* Execute script *}
     if (code <> TCL_OK) then                   {* Is all okay? *}
        writeln(Tcl_GetStringResult(interp));

     {* Call a function `foo` defined in the script *}
     code := Tcl_VarEval(interp,['foo ','1 2 3',nil]);
     if (code <> TCL_OK) then
        writeln(Tcl_GetStringResult(interp));
     Tcl_DeleteInterp(interp);                  {* Release interpreter *}
end.
