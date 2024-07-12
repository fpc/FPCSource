{
    Copyright (c) 1998-2008 by Carl Eric Codere and Peter Vreman
    Copyright (c) 2024 by Nikolay Nikolov

    Does the parsing for the WebAssembly styled inline assembler.

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
Unit rawasmtext;

{$i fpcdefs.inc}

  Interface

    uses
      cclasses,
      globtype,
      rasm,rawasm,
      aasmbase,cpubase;

    type
      tasmtoken = (
        AS_NONE,AS_LPAREN,AS_RPAREN,AS_ID,AS_END,AS_OPCODE,AS_INTNUM,
        AS_REALNUM,AS_STRING
      );
      tasmkeyword = string[10];

    const
      token2str : array[tasmtoken] of tasmkeyword=(
        '','(',')','identifier','end','opcode','integer','float','string');

    type

      { twasmreader }

      twasmreader = class(tasmreader)
      private
        actasmpattern_origcase: string;
        actasmtoken   : tasmtoken;
        prevasmtoken  : tasmtoken;
        procedure SetupTables;
        procedure GetToken;
        function consume(t : tasmtoken):boolean;
        function is_asmopcode(const s: string):boolean;
      public
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
      cgbase,cgutils,cgobj,
      { wasm }
      itcpuwasm
      ;


{*****************************************************************************
                                twasmreader
*****************************************************************************}

    procedure twasmreader.SetupTables;
      var
        i: TAsmOp;
      begin
        iasmops:=TFPHashList.create;
        for i:=firstop to lastop do
          if wasm_op2str[i]<>'end' then
            iasmops.Add(wasm_op2str[i],Pointer(PtrInt(i)));
      end;


    procedure twasmreader.GetToken;
      var
        len: Integer;
        has_sign, is_hex, is_float: Boolean;
        tmpS: string;
        tmpI, tmpCode: Integer;
      begin
        c:=scanner.c;
        { save old token and reset new token }
        prevasmtoken:=actasmtoken;
        actasmtoken:=AS_NONE;
        { reset }
        actasmpattern:='';
        { while space, tab, new line or comment, continue scan... }
        while c in [' ',#9,#13,#10] do
          begin
            c:=current_scanner.asmgetchar;
            case c of
              ';':
                begin
                  c:=current_scanner.asmgetchar;
                  case c of
                    { ;; comment until end of line }
                    ';':
                      begin
                        { skip until end of line }
                        repeat
                          c:=current_scanner.asmgetchar;
                        until c in [#13,#10];
                      end;
                    else
                      current_scanner.illegal_char(c);
                  end;
                end;
              '(':
                begin
                  current_scanner.gettokenpos;
                  c:=current_scanner.asmgetchar;
                  case c of
                    { (; block comment ;) }
                    ';':
                      begin
                        { skip until ;) }
                        repeat
                          c:=current_scanner.asmgetchar;
                          if c=';' then
                            begin
                              c:=current_scanner.asmgetchar;
                              if c=')' then
                                begin
                                  c:=current_scanner.asmgetchar;
                                  break;
                                end;
                            end;
                        until false;
                      end;
                    else
                      begin
                        actasmtoken:=AS_LPAREN;
                        exit;
                      end;
                  end;
                end;
            end;
          end;
        current_scanner.gettokenpos;
        case c of
          ')':
            begin
              c:=current_scanner.asmgetchar;
              actasmtoken:=AS_RPAREN;
            end;
          '$','a'..'z','A'..'Z':
            begin
              len:=0;
              while c in ['A'..'Z','a'..'z','0'..'9',
                          '!','#','$','%','&','''','*','+','-','.','/',
                          ':','<','=','>','?','@','\','^','_','`','|','~'] do
                begin
                  inc(len);
                  actasmpattern[len]:=c;
                  c:=current_scanner.asmgetchar;
                end;
              actasmpattern[0]:=chr(len);
              actasmpattern_origcase:=actasmpattern;
              if actasmpattern[1]='$' then
                actasmtoken:=AS_ID
              else if is_asmopcode(actasmpattern) then
                begin
                  exit;
                end
              else if upper(actasmpattern) = 'END' then
                begin
                  uppervar(actasmpattern);
                  actasmtoken:=AS_END;
                  exit;
                end
              else
                begin
                  message1(asmr_e_unknown_opcode,actasmpattern);
                  actasmtoken:=AS_NONE;
                end;
            end;
          '0'..'9','+','-':
            begin
              len:=0;
              has_sign:=false;
              is_hex:=false;
              is_float:=false;
              if c in ['+','-'] then
                begin
                  has_sign:=true;
                  inc(len);
                  actasmpattern[len]:=c;
                  c:=current_scanner.asmgetchar;
                end;
              if c='0' then
                begin
                  inc(len);
                  actasmpattern[len]:=c;
                  c:=current_scanner.asmgetchar;
                  if c='x' then
                    begin
                      is_hex:=true;
                      inc(len);
                      actasmpattern[len]:=c;
                      c:=current_scanner.asmgetchar;
                    end;
                end;
              if is_hex then
                begin
                  while c in ['0'..'9','a'..'f','A'..'F'] do
                    begin
                      inc(len);
                      actasmpattern[len]:=c;
                      c:=current_scanner.asmgetchar;
                    end;
                end
              else
                begin
                  while c in ['0'..'9'] do
                    begin
                      inc(len);
                      actasmpattern[len]:=c;
                      c:=current_scanner.asmgetchar;
                    end;
                end;
              if c='.' then
                begin
                  is_float:=true;
                  inc(len);
                  actasmpattern[len]:=c;
                  c:=current_scanner.asmgetchar;
                  { parse the fractional part }
                  if is_hex then
                    begin
                      while c in ['0'..'9','a'..'f','A'..'F'] do
                        begin
                          inc(len);
                          actasmpattern[len]:=c;
                          c:=current_scanner.asmgetchar;
                        end;
                    end
                  else
                    begin
                      while c in ['0'..'9'] do
                        begin
                          inc(len);
                          actasmpattern[len]:=c;
                          c:=current_scanner.asmgetchar;
                        end;
                    end;
                end;
              if (is_hex and (c in ['p','P'])) or
                 ((not is_hex) and (c in ['e','E'])) then
                begin
                  inc(len);
                  actasmpattern[len]:=c;
                  c:=current_scanner.asmgetchar;
                  if c in ['+','-'] then
                    begin
                      inc(len);
                      actasmpattern[len]:=c;
                      c:=current_scanner.asmgetchar;
                    end;
                  while c in ['0'..'9'] do
                    begin
                      inc(len);
                      actasmpattern[len]:=c;
                      c:=current_scanner.asmgetchar;
                    end;
                end;
              actasmpattern[0]:=chr(len);
              if is_float then
                actasmtoken:=AS_REALNUM
              else
                actasmtoken:=AS_INTNUM;
            end;
          '"':
            begin
              actasmpattern:='';
              repeat
                c:=current_scanner.asmgetchar;
                case c of
                  '\' :
                    begin
                      c:=current_scanner.asmgetchar;
                      case c of
                        't':
                          begin
                            actasmpattern:=actasmpattern+#9;
                            c:=current_scanner.asmgetchar;
                          end;
                        'n':
                          begin
                            actasmpattern:=actasmpattern+#10;
                            c:=current_scanner.asmgetchar;
                          end;
                        'r':
                          begin
                            actasmpattern:=actasmpattern+#13;
                            c:=current_scanner.asmgetchar;
                          end;
                        '"':
                          begin
                            actasmpattern:=actasmpattern+'"';
                            c:=current_scanner.asmgetchar;
                          end;
                        '''':
                          begin
                            actasmpattern:=actasmpattern+'''';
                            c:=current_scanner.asmgetchar;
                          end;
                        '\':
                          begin
                            actasmpattern:=actasmpattern+'\';
                            c:=current_scanner.asmgetchar;
                          end;
                        'u':
                          begin
                            tmpS:='';
                            c:=current_scanner.asmgetchar;
                            while c in ['0'..'9','a'..'f','A'..'F'] do
                              begin
                                tmpS:=tmpS+c;
                                c:=current_scanner.asmgetchar;
                              end;
                            if tmpS<>'' then
                              begin
                                Val('$'+tmpS,tmpI,tmpCode);
                                if (tmpI<$D800) or ((tmpI>=$E000) and (tmpI<$110000)) then
                                  begin
                                    if tmpI<=$7F then
                                      actasmpattern:=actasmpattern+Chr(tmpI)
                                    else if tmpI<=$7FF then
                                      actasmpattern:=actasmpattern+
                                        Chr(%11000000 or (tmpI shr 6))+
                                        Chr(%10000000 or (tmpI and $3F))
                                    else if tmpI<=$FFFF then
                                      actasmpattern:=actasmpattern+
                                        Chr(%11100000 or (tmpI shr 12))+
                                        Chr(%10000000 or ((tmpI shr 6) and $3F))+
                                        Chr(%10000000 or (tmpI and $3F))
                                    else
                                      actasmpattern:=actasmpattern+
                                        Chr(%11110000 or (tmpI shr 18))+
                                        Chr(%10000000 or ((tmpI shr 12) and $3F))+
                                        Chr(%10000000 or ((tmpI shr 6) and $3F))+
                                        Chr(%10000000 or (tmpI and $3F))
                                  end
                                else
                                  Message1(asmr_e_escape_seq_ignored,'u'+tmpS);
                              end
                            else
                              Message1(asmr_e_escape_seq_ignored,'u');
                          end;
                        '0'..'9','a'..'f','A'..'F':
                          begin
                            tmpS:=c;
                            c:=current_scanner.asmgetchar;
                            if c in ['0'..'9','a'..'f','A'..'F'] then
                              begin
                                tmpS:=tmpS+c;
                                c:=current_scanner.asmgetchar;
                                Val('$'+tmpS,tmpI,tmpCode);
                                actasmpattern:=actasmpattern+Chr(tmpI);
                              end
                            else
                              begin
                                Message1(asmr_e_escape_seq_ignored,tmpS+c);
                                c:=current_scanner.asmgetchar;
                              end;
                          end;
                        else
                          begin
                            Message1(asmr_e_escape_seq_ignored,c);
                            c:=current_scanner.asmgetchar;
                          end;
                      end;
                    end;
                  '"' :
                    begin
                      c:=current_scanner.asmgetchar;
                      break;
                    end;
                  #10,#13:
                    Message(scan_f_string_exceeds_line);
                  #0..#9,#11,#12,#14..#31,#127:
                    current_scanner.illegal_char(c);
                  else
                    actasmpattern:=actasmpattern+c;
                end;
              until false;
              actasmtoken:=AS_STRING;
              exit;
            end;
          else
            current_scanner.illegal_char(c);
        end;
      end;


    function twasmreader.consume(t: tasmtoken): boolean;
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


    function twasmreader.is_asmopcode(const s: string): boolean;
      begin
        actopcode:=tasmop(PtrUInt(iasmops.Find(s)));
        if actopcode<>A_NONE then
          begin
            actasmtoken:=AS_OPCODE;
            is_asmopcode:=true;
          end
        else
          is_asmopcode:=false;
      end;


    function twasmreader.Assemble: tlinkedlist;
      begin
        Message1(asmr_d_start_reading,'WebAssembly');
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
        { main loop }
        repeat
          Writeln(actasmtoken);
          case actasmtoken of
            AS_END:
              break; { end assembly block }
            else
              begin
                Consume(actasmtoken);
                //Message(asmr_e_syntax_error);
                //RecoverConsume(false);
              end;
          end;
        until false;

        { Return the list in an asmnode }
        assemble:=curlist;
        Message1(asmr_d_finish_reading,'WebAssembly');
      end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

const
  asmmode_wasm_standard_info : tasmmodeinfo =
          (
            id    : asmmode_standard;
            idtxt : 'STANDARD';
            casmreader : twasmreader;
          );

initialization
  RegisterAsmMode(asmmode_wasm_standard_info);
end.
