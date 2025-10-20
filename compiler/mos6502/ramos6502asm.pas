{
    Copyright (c) 2025 by Nikolay Nikolov

    Does the parsing for the MOS Technology 6502 styled inline assembler.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
Unit ramos6502asm;

{$i fpcdefs.inc}

  Interface

    uses
      cclasses,
      globtype,
      rasm,ramos6502,
      aasmbase,cpubase;

    type
      tasmtoken = (
        AS_NONE,AS_LABEL,AS_LLABEL,AS_STRING,AS_INTNUM,
        AS_REALNUM,AS_COMMA,AS_LPAREN,
        AS_RPAREN,AS_COLON,AS_DOT,AS_PLUS,AS_MINUS,AS_STAR,
        AS_SEPARATOR,AS_ID,AS_REGISTER,AS_OPCODE,AS_CONDITION,AS_SLASH,AS_DOLLAR,
        AS_HASH,AS_LSBRACKET,AS_RSBRACKET,AS_LBRACKET,AS_RBRACKET,
        AS_EQUAL,
        {------------------ Assembler directives --------------------}
        AS_DEFB,AS_DEFW,AS_AREA,AS_END,
        {------------------ Assembler Operators  --------------------}
        AS_TYPE,AS_OFFSET,AS_SIZEOF,AS_VMTOFFSET,AS_MOD,AS_SHL,AS_SHR,AS_NOT,AS_AND,AS_OR,AS_XOR,AS_NOR,AS_AT,
        AS_RELTYPE, // common token for relocation types
        {------------------ Target-specific directive ---------------}
        AS_TARGET_DIRECTIVE
        );
      tasmkeyword = string[10];

    const
      { These tokens should be modified accordingly to the modifications }
      { in the different enumerations.                                   }
      firstdirective = AS_DEFB;
      lastdirective  = AS_END;
      token2str : array[tasmtoken] of tasmkeyword=(
        '','Label','LLabel','string','integer',
        'float',',','(',
        ')',':','.','+','-','*',
        ';','identifier','register','opcode','condition','/','$',
        '#','{','}','[',']',
        '=',
        'defb','defw','area','END',
        'TYPE','OFFSET','SIZEOF','VMTOFFSET','%','<<','>>','!','&','|','^','~','@','reltype',
        'directive');

    type

      { tmos6502reader }

      tmos6502reader = class(tasmreader)
        actasmcond : TAsmCond;
        actasmpattern_origcase : string;
        actasmtoken   : tasmtoken;
        prevasmtoken  : tasmtoken;
        procedure SetupTables;
        procedure GetToken;
        function consume(t : tasmtoken):boolean;
        procedure RecoverConsume(allowcomma:boolean);
        function is_asmopcode(const s: string):boolean;
        function Assemble: tlinkedlist;override;
      end;


  Implementation

    uses
      { helpers }
      cutils,
      { global }
      globals,verbose,
      systems,
      { aasm }
      cpuinfo,aasmtai,aasmdata,aasmcpu,
      { symtable }
      symconst,symbase,symtype,symsym,symtable,symdef,symutil,
      { parser }
      scanner,pbase,
      procinfo,
      rabase,rautils,
      cgbase,cgutils,cgobj
      ;


{*****************************************************************************
                                tmos6502reader
*****************************************************************************}


    procedure tmos6502reader.SetupTables;
      var
        i: TAsmOp;
        cond: TAsmCond;
      begin
        iasmops:=TFPHashList.create;
        for i:=firstop to lastop do
          if i<>A_Bxx then
            iasmops.Add(upper(std_op2str[i]),Pointer(PtrInt(i)));
        for cond in TAsmCond do
          if cond<>C_None then
            iasmops.Add('B'+uppercond2str[cond],Pointer(PtrInt(A_Bxx)));
      end;


    procedure tmos6502reader.GetToken;
      var
        len: Integer;
      begin
        c:=scanner.c;
        { save old token and reset new token }
        prevasmtoken:=actasmtoken;
        actasmtoken:=AS_NONE;
        { reset }
        actasmpattern:='';
        { while space and tab , continue scan... }
        while c in [' ',#9] do
          c:=current_scanner.asmgetchar;
        { get token pos }
        if not (c in [#10,#13,'{',';','/','(']) then
          current_scanner.gettokenpos;
        { Local Label, Label, Directive, Prefix or Opcode }
        if firsttoken and not(c in [#10,#13,';']) then
          begin
            firsttoken:=FALSE;
            len:=0;
            { only opcodes, global and local labels are allowed now. }
            while c in ['A'..'Z','a'..'z','0'..'9','_','@'] do
              begin
                inc(len);
                actasmpattern[len]:=c;
                c:=current_scanner.asmgetchar;
              end;
            actasmpattern[0]:=chr(len);
            actasmpattern_origcase:=actasmpattern;
            { Opcode ? }
            if is_asmopcode(upper(actasmpattern)) then
              begin
                uppervar(actasmpattern);
                exit;
              end;
            { End of assemblerblock ? }
            if upper(actasmpattern) = 'END' then
              begin
                actasmtoken:=AS_END;
                exit;
              end;
            message1(asmr_e_unknown_opcode,actasmpattern);
            actasmtoken:=AS_NONE;
          end
        else { else firsttoken }
          { Here we must handle all possible cases }
          begin
            case c of
            { identifier, register, prefix or directive }
               '_','A'..'Z','a'..'z':
                 begin
                   len:=0;
                   while c in ['A'..'Z','a'..'z','0'..'9','_','$'] do
                    begin
                      inc(len);
                      actasmpattern[len]:=c;
                      c:=current_scanner.asmgetchar;
                    end;
                   actasmpattern[0]:=chr(len);
                   actasmpattern_origcase:=actasmpattern;
                   uppervar(actasmpattern);
                   { check for end which is a reserved word unlike the opcodes }
                   if actasmpattern = 'END' then
                     begin
                       actasmtoken:=AS_END;
                       exit;
                     end;
                   //if is_register(actasmpattern) then
                   //  begin
                   //    actasmtoken:=AS_REGISTER;
                   //    { is it an alternate register? }
                   //    if (c='''') and is_register(actasmpattern+'''') then
                   //      begin
                   //        actasmpattern:=actasmpattern+c;
                   //        c:=current_scanner.asmgetchar;
                   //      end;
                   //    exit;
                   //  end;
                   { if next is a '.' and this is a unitsym then we also need to
                     parse the identifier }
                   //if (c='.') then
                   // begin
                   //   searchsym(actasmpattern,srsym,srsymtable);
                   //   if assigned(srsym) and
                   //      (srsym.typ=unitsym) and
                   //      (srsym.owner.symtabletype in [staticsymtable,globalsymtable]) and
                   //      srsym.owner.iscurrentunit then
                   //    begin
                   //      actasmpattern:=actasmpattern+c;
                   //      c:=current_scanner.asmgetchar;
                   //      while c in  ['A'..'Z','a'..'z','0'..'9','_','$'] do
                   //       begin
                   //         actasmpattern:=actasmpattern + upcase(c);
                   //         c:=current_scanner.asmgetchar;
                   //       end;
                   //    end;
                   // end;
                   actasmtoken:=AS_ID;
                   exit;
                 end;

               #13,#10:
                 begin
                   current_scanner.linebreak;
                   c:=current_scanner.asmgetchar;
                   firsttoken:=TRUE;
                   actasmtoken:=AS_SEPARATOR;
                   exit;
                 end;

               ';' :
                 begin
                   c:=current_scanner.asmgetchar;
                   firsttoken:=TRUE;
                   actasmtoken:=AS_SEPARATOR;
                   exit;
                 end;

               else
                 current_scanner.illegal_char(c);
            end;
          end;
      end;


    function tmos6502reader.consume(t : tasmtoken):boolean;
      begin
        Consume:=true;
        if t<>actasmtoken then
         begin
           Message2(scan_f_syn_expected,token2str[t],token2str[actasmtoken]);
           Consume:=false;
         end;
        repeat
          gettoken;
        until actasmtoken<>AS_NONE;
      end;


    procedure tmos6502reader.RecoverConsume(allowcomma:boolean);
      begin
        while not (actasmtoken in [AS_SEPARATOR,AS_END]) do
          begin
            if allowcomma and (actasmtoken=AS_COMMA) then
             break;
            Consume(actasmtoken);
          end;
      end;


    function tmos6502reader.is_asmopcode(const s: string):boolean;
      var
        cond: TAsmCond;
        condstr: String;
      begin
        actcondition:=C_None;
        actopcode:=tasmop(PtrUInt(iasmops.Find(s)));
        if actopcode<>A_NONE then
          begin
            actasmtoken:=AS_OPCODE;
            is_asmopcode:=true;
            if actopcode=A_Bxx then
              begin
                condstr:=Copy(s,2,2);
                for cond in TAsmCond do
                  if (cond<>C_None) and (uppercond2str[cond]=condstr) then
                    begin
                      actasmcond:=cond;
                      exit;
                    end;
              end;
          end
        else
          is_asmopcode:=false;
      end;


    function tmos6502reader.Assemble: tlinkedlist;
      var
        hl: tasmlabel;
        sectionname: String;
        section: tai_section;
        lastsectype : TAsmSectiontype;
      begin
        Message1(asmr_d_start_reading,'MOS Technology 6502');
        firsttoken:=TRUE;
        { sets up all opcode and register tables in uppercase }
        if not _asmsorted then
          begin
            SetupTables;
            _asmsorted:=TRUE;
          end;
        curlist:=TAsmList.Create;

        { we might need to know which parameters are passed in registers }
        if not parse_generic then
          current_procinfo.generate_parameter_info;

        { start tokenizer }
        gettoken;
        lastsectype:=sec_code;
        { main loop }
        //repeat
        //  case actasmtoken of
        //    AS_LLABEL:
        //      Begin
        //        if CreateLocalLabel(actasmpattern,hl,true) then
        //          ConcatLabel(curlist,hl);
        //        Consume(AS_LLABEL);
        //      end;
        //
        //    AS_LABEL:
        //      Begin
        //        if SearchLabel(upper(actasmpattern),hl,true) then
        //          begin
        //            if hl.is_public then
        //              ConcatPublic(curlist,actasmpattern_origcase);
        //            ConcatLabel(curlist,hl);
        //          end
        //        else
        //         Message1(asmr_e_unknown_label_identifier,actasmpattern);
        //        Consume(AS_LABEL);
        //      end;
        //
        //    AS_END:
        //      begin
        //        break; { end assembly block }
        //      end;
        //
        //    AS_SEPARATOR:
        //      begin
        //        Consume(AS_SEPARATOR);
        //      end;
        //
        //    AS_DEFB :
        //      Begin
        //        inexpression:=true;
        //        Consume(AS_DEFB);
        //        BuildConstant(1);
        //        inexpression:=false;
        //      end;
        //
        //    AS_DEFW :
        //      Begin
        //        inexpression:=true;
        //        Consume(AS_DEFW);
        //        BuildConstant(2);
        //        inexpression:=false;
        //      end;
        //
        //    AS_AREA :
        //      begin
        //        Consume(AS_AREA);
        //        sectionname:=actasmpattern;
        //        {secflags:=[];
        //        secprogbits:=SPB_None;}
        //        Consume(AS_STRING);
        //        {if actasmtoken=AS_COMMA then
        //          begin
        //            Consume(AS_COMMA);
        //            if actasmtoken=AS_STRING then
        //              begin
        //                case actasmpattern of
        //                  'a':
        //                    Include(secflags,SF_A);
        //                  'w':
        //                    Include(secflags,SF_W);
        //                  'x':
        //                    Include(secflags,SF_X);
        //                  '':
        //                    ;
        //                  else
        //                    Message(asmr_e_syntax_error);
        //                end;
        //                Consume(AS_STRING);
        //                if actasmtoken=AS_COMMA then
        //                  begin
        //                    Consume(AS_COMMA);
        //                    if (actasmtoken=AS_MOD) or (actasmtoken=AS_AT) then
        //                      begin
        //                        Consume(actasmtoken);
        //                        if actasmtoken=AS_ID then
        //                          begin
        //                            case actasmpattern of
        //                              'PROGBITS':
        //                                secprogbits:=SPB_PROGBITS;
        //                              'NOBITS':
        //                                secprogbits:=SPB_NOBITS;
        //                              'NOTE':
        //                                secprogbits:=SPB_NOTE;
        //                              else
        //                                Message(asmr_e_syntax_error);
        //                            end;
        //                            Consume(AS_ID);
        //                          end
        //                        else
        //                          Message(asmr_e_syntax_error);
        //                      end
        //                    else
        //                      Message(asmr_e_syntax_error);
        //                  end;
        //              end
        //            else
        //              Message(asmr_e_syntax_error);
        //          end;}
        //
        //        //curList.concat(tai_section.create(sec_user, actasmpattern, 0));
        //        section:=new_section(curlist, sec_user, sectionname, 0);
        //        lastsectype:=sec_user;
        //        //section.secflags:=secflags;
        //        //section.secprogbits:=secprogbits;
        //      end;
        //
        //    AS_OPCODE:
        //      begin
        //        HandleOpCode;
        //      end;
        //
        //    else
        //      begin
        //        Message(asmr_e_syntax_error);
        //        RecoverConsume(false);
        //      end;
        //  end;
        //until false;
        { are we back in the code section? }
        if lastsectype<>sec_code then
          begin
            //Message(asmr_w_assembler_code_not_returned_to_text);
            new_section(curList,sec_code,lower(current_procinfo.procdef.mangledname),0);
          end;
        { check that all referenced local labels are defined }
        checklocallabels;
        { Return the list in an asmnode }
        assemble:=curlist;
        Message1(asmr_d_finish_reading,'MOS Technology 6502');
      end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

const
  asmmode_mos6502_standard_info : tasmmodeinfo =
          (
            id    : asmmode_standard;
            idtxt : 'STANDARD';
            casmreader : tmos6502reader;
          );

initialization
  RegisterAsmMode(asmmode_mos6502_standard_info);
end.
