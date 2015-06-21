program mkarminl;

{$mode objfpc}
{$H+}

uses
  sysutils, classes,
  strutils;

type
  TOperDirection = (operIn, operVar, operOut);

  TOperand = record
    name,
    namehi, // If this is not empty the operand is a virtual register pair operand
    typ: string;
    direction: TOperDirection;
  end;

const
  DirLUT: array[TOperDirection] of string = ('','var ','out ');

function GetPascalType(const ATyp: string): string;
  begin
    case ATyp of
      'r32':   exit('longword');
      'rs32':  exit('longint');
      'r64':   exit('qword');
      'rs64':  exit('int64');
      'i32':   exit('longint');
      'ptr32': exit('pointer');
    else
      exit(ATyp);
    end;
  end;

function GetTypeDef(const ATyp: string): string;
  begin
    case ATyp of
      'r32':   exit('u32inttype');
      'rs32':  exit('s32inttype');
      'r64':   exit('u64inttype');
      'rs64':  exit('s64inttype');
      'i32':   exit('s32inttype');
      'ptr32': exit('voidpointertype');
    else
      exit(ATyp);
    end;
  end;

function GetOper(const ATyp: string): string;
  begin
    case ATyp of
      'r32':   exit('_reg');
      'rs32':  exit('_reg');
      'r64':   exit('_reg_reg');
      'rs64':  exit('_reg_reg');
      'i32':   exit('_const');
      'ptr32': exit('_ref');
    else
      exit('');
    end;
  end;

function GetOperand(const ATyp: string; AIndex: longint): string;
  begin
    case ATyp of
      'r32':   exit(format(',paraarray[%d].location.register', [AIndex]));
      'rs32':  exit(format(',paraarray[%d].location.register', [AIndex]));
      'r64':   exit(format(',paraarray[%d].location.register64.reglo,paraarray[%d].location.register64.reghi', [AIndex,AIndex]));
      'rs64':  exit(format(',paraarray[%d].location.register64.reglo,paraarray[%d].location.register64.reghi', [AIndex,AIndex]));
      'i32':   exit(format(',GetConstInt(paraarray[%d])',[AIndex]));
      'ptr32': exit(format(',paraarray[%d].location.reference', [AIndex]));
    else
      exit(ATyp);
    end;
  end;

function GetOperandLoc(const ATyp: string): string;
  begin
    result:='';
    case ATyp of
      'r32':  exit(',location.register');
      'rs32': exit(',location.register');
      'r64':  exit(',location.register64.reglo,location.register64.reghi');
      'rs64': exit(',location.register64.reglo,location.register64.reghi');
    end;
  end;

function GetLocStatement(AIndex: longint; const ATyp: string; AConst: boolean): string;
  begin
    result:='';
    case ATyp of
      'r32':   exit(format('hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[%d].location, paraarray[%d].resultdef,u32inttype,%s);', [AIndex+1, AIndex+1, BoolToStr(aconst,'true','false')]));
      'rs32':  exit(format('hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[%d].location, paraarray[%d].resultdef,u32inttype,%s);', [AIndex+1, AIndex+1, BoolToStr(aconst,'true','false')]));
      'r64':   exit(format('hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[%d].location, paraarray[%d].resultdef,u64inttype,%s);', [AIndex+1, AIndex+1, BoolToStr(aconst,'true','false')]));
      'rs64':  exit(format('hlcg.location_force_reg(current_asmdata.CurrAsmList, paraarray[%d].location, paraarray[%d].resultdef,u64inttype,%s);', [AIndex+1, AIndex+1, BoolToStr(aconst,'true','false')]));
      'ptr32': exit(format('location_make_ref(paraarray[%d].location);', [AIndex+1]));
    end;
  end;

function GetLoc(const ATyp: string): string;
  begin
    result:='';
    case ATyp of
      'r32':   exit('LOC_REGISTER,OS_32');
      'rs32':  exit('LOC_REGISTER,OS_S32');
      'r64':   exit('LOC_REGISTER,OS_64');
      'rs64':  exit('LOC_REGISTER,OS_S64');
      'ptr32': exit('LOC_MEM,OS_32');
    end;
  end;

function GetLocAllocation(const ATyp: string): string;
  begin
    result:='';
    case ATyp of
      'r32':  exit('location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);');
      'rs32': exit('location.register:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);');
      'r64':  exit('location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList, OS_32); location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);');
      'rs64': exit('location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList, OS_32); location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList, OS_32);');
    end;
  end;

function GetPostFix(const APF: string): string;
  begin
    if APF<>'' then
      result:='PF_'+APF
    else
      result:='PF_None';
  end;

procedure ParseList(const APrefix, AFilename: string);
  var
    f: TextFile;

    fprocs,
    finnr: TextFile;
    ftypechk, ffirst, fsecond: TStringList;

    str,
    instrPart,postfix,_alias,
    params, operline: String;

    opers: array[0..7] of TOperand;
    opercnt: longint;

    hasOutput: boolean;
    outputType: string;
    cnt,
    i, intrnum: longint;
    tmp: String;

  function ParseOperands(AIndex: longint = -1): string;
    var
      idx: LongInt;
      pt: Integer;
      c: Char;
    begin
      idx:=opercnt;

      params:=trim(params);
      if params='' then
        exit('');

      inc(opercnt);

      if pos('var ', params)=1 then
        begin
          opers[idx].direction:=operVar;
          Delete(params,1,4);
          params:=trim(params);
          hasOutput:=true;
        end
      else if pos('out ', params)=1 then
        begin
          opers[idx].direction:=operOut;
          Delete(params,1,4);
          params:=trim(params);
          hasOutput:=true;
        end
      else
        begin
          if AIndex<>-1 then
            opers[idx].direction:=opers[AIndex].direction
          else
            opers[idx].direction:=operIn;
        end;

      if pos('[',params)=1 then
        begin
          delete(params,1,1);
          opers[idx].name:=Copy2SymbDel(params, ',');
          opers[idx].namehi:=Copy2SymbDel(params, ']');

          pt:=PosSet([',',':'], params);
          c:=params[pt];

          Copy2SymbDel(params,c);

          params:=trim(params);
        end
      else
        begin
          pt:=PosSet([',',':'], params);

          c:=params[pt];
          opers[idx].name:=Copy2SymbDel(params, c);
          opers[idx].namehi:='';
          params:=trim(params);
        end;

      if c = ':' then
        begin
          opers[idx].typ:=Copy2SymbDel(params, ';');
          result:=opers[idx].typ;
        end
      else
        begin
          opers[idx].typ:=ParseOperands(idx);
          result:=opers[idx].typ;
        end;

      if opers[idx].direction<>operIn then
        outputType:=opers[idx].typ;
    end;

  function GetOperLine: string;
    var
      i: longint;
    begin
      result:='';
      for i := 0 to opercnt-1 do
        if opers[i].namehi<>'' then
          result:=result+DirLUT[opers[i].direction]+opers[i].name+'-'+opers[i].namehi+':'+opers[i].typ+';'
        else
          result:=result+DirLUT[opers[i].direction]+opers[i].name+':'+opers[i].typ+';';
    end;

  function GetParams: longint;
    var
      i: longint;
    begin
      result:=0;
      for i := 0 to opercnt-1 do
        if opers[i].direction in [operIn,operVar] then
          inc(result);
    end;

  function FindOperIdx(const AOper: string): longint;
    var
      i,cnt: longint;
    begin
      cnt:=0;
      result:=0;
      for i := 0 to opercnt-1 do
        if (opers[i].direction in [operIn,operVar]) then
          begin
            if opers[i].name=AOper then
              exit(cnt);
            inc(cnt);
          end;
    end;


  const
    headercomment = '{'+LineEnding+
                    '     Do not edit file manually!'+LineEnding+
                    '     File is created automatically from %s by mkarminl.'+LineEnding+
                    '}'+LineEnding;

  begin
    intrnum:=0;

    assignfile(f, AFilename);
    reset(f);

    assignfile(fprocs, APrefix+'procs.inc'); rewrite(fprocs); writeln(fprocs,format(headercomment,[AFilename]));
    assignfile(finnr, APrefix+'innr.inc'); rewrite(finnr); writeln(finnr,format(headercomment,[AFilename]));

    writeln(finnr,'const');

    ftypechk:=TStringList.Create;
    ffirst:=TStringList.Create;
    fsecond:=TStringList.Create;

    writeln(finnr, '  in_', APrefix,'_first = in_',APrefix,'_base;');

    while not EOF(f) do
      begin
        readln(f, str);

        str:=trim(str);

        if (str='') or (Pos(';',str)=1) then
          continue;

        instrPart:=Copy2SymbDel(str, '(');

        // Check for postfix
        if pos('{',instrPart)>0 then
          begin
            postfix:=instrPart;
            instrPart:=Copy2SymbDel(postfix, '{');
            postfix:=TrimRightSet(postfix,['}']);
          end
        else
          postfix:='';

        // Check for alias
        if pos('[',instrPart)>0 then
          begin
            _alias:=instrPart;
            instrPart:=Copy2SymbDel(_alias, '[');
            _alias:='_'+TrimRightSet(_alias,[']']);
          end
        else
          _alias:='';

        // Get parameters
        params:=trim(Copy2SymbDel(str,')'));
        str:=trim(str);

        hasOutput:=false;
        opercnt:=0;
        outputType:='';

        while params<>'' do
          ParseOperands;

        operline:=GetOperLine;
        // Write typecheck code
        i:=ftypechk.IndexOf(': //'+operline);
        if i>=0 then
          ftypechk.Insert(i,',in_'+APrefix+'_'+instrPart+postfix+_alias)
        else
          begin
            ftypechk.Add('in_'+APrefix+'_'+instrPart+postfix+_alias);
            ftypechk.Add(': //'+operline);
            ftypechk.Add('  begin');
            ftypechk.Add('    CheckParameters('+inttostr(GetParams())+');');
            if hasOutput then
              ftypechk.Add('    resultdef:='+GetTypeDef(outputType)+';')
            else
              ftypechk.Add('    resultdef:=voidtype;');
            ftypechk.Add('  end;')
          end;

        // Write firstpass code
        i:=ffirst.IndexOf(': //'+operline);
        if i>=0 then
          ffirst.Insert(i,',in_'+APrefix+'_'+instrPart+postfix+_alias)
        else
          begin
            ffirst.Add('in_'+APrefix+'_'+instrPart+postfix+_alias);
            ffirst.Add(': //'+operline);
            ffirst.Add('  begin');
            if hasOutput then
              ffirst.Add('    expectloc:=LOC_REGISTER;')
            else
              ffirst.Add('    expectloc:=LOC_VOID;');
            ffirst.Add('    result:=nil;');
            ffirst.Add('  end;')
          end;

        // Write secondpass code
        i:=fsecond.IndexOf(': //'+operline);
        if i>=0 then
          begin
            fsecond.Insert(i+3,'      in_'+APrefix+'_'+instrPart+postfix+_alias+': begin op:=A_'+instrPart+'; pf:='+GetPostFix(postfix)+'; end;');
            fsecond.Insert(i,',in_'+APrefix+'_'+instrPart+postfix+_alias);
          end
        else
          begin
            fsecond.Add('in_'+APrefix+'_'+instrPart+postfix+_alias);
            fsecond.Add(': //'+operline);
            fsecond.Add('  begin');
            fsecond.add('    case inlinenumber of');
            fsecond.Add('      in_'+APrefix+'_'+instrPart+postfix+_alias+': begin op:=A_'+instrPart+'; pf:='+GetPostFix(postfix)+'; end;');
            fsecond.add('    end;');
            fsecond.Add('');

            i:=GetParams;
            fsecond.Add('    GetParameters('+inttostr(i)+');');
            fsecond.Add('');

            fsecond.add('    for i := 1 to '+inttostr(i)+' do secondpass(paraarray[i]);');
            fsecond.Add('');

            // Force inputs
            cnt:=0;
            for i := 0 to opercnt-1 do
              begin
                case opers[i].direction of
                  operIn:
                    begin
                      tmp:=GetLocStatement(cnt, opers[i].typ, true);
                      if tmp<>'' then
                        fsecond.add('    '+tmp);
                      inc(cnt);
                    end;
                  operVar:
                    begin
                      tmp:=GetLocStatement(cnt, opers[i].typ, false);
                      if tmp<>'' then
                        fsecond.add('    '+tmp);
                      inc(cnt);
                    end;
                end;
              end;

            // Allocate output
            cnt:=0;
            for i := 0 to opercnt-1 do
              begin
                case opers[i].direction of
                  operOut:
                    begin
                      if opers[i].namehi<>'' then
                        begin
                          fsecond.add('    location_reset(location,'+GetLoc(opers[i].typ)+');');
                          fsecond.Add('    location.register64.reglo:=paraarray['+inttostr(FindOperIdx(opers[i].name)+1)+'].location.register;');
                          fsecond.Add('    location.register64.reghi:=paraarray['+inttostr(FindOperIdx(opers[i].namehi)+1)+'].location.register;');
                        end
                      else
                        begin
                          fsecond.add('    location_reset(location,'+GetLoc(opers[i].typ)+');');
                          fsecond.Add('    '+GetLocAllocation(opers[i].typ));
                        end;
                    end;
                  operVar:
                    begin
                      if opers[i].namehi<>'' then
                        begin
                          fsecond.add('    location_reset(location,'+GetLoc(opers[i].typ)+');');
                          fsecond.Add('    location.register64:=paraarray['+inttostr(cnt+1)+'].location.register64;');
                        end
                      else
                        begin
                          //fsecond.add('    location_reset(location,'+GetLoc(opers[i].typ)+');');
                          //fsecond.Add('    location.register:=paraarray['+inttostr(cnt+1)+'].location.register;');
                          fsecond.Add('    location:=paraarray['+inttostr(cnt+1)+'].location;');
                        end;
                      inc(cnt);
                    end;
                  operIn:
                    inc(cnt);
                end;
              end;

            operline:='taicpu.op';
            for i := 0 to opercnt-1 do
              begin
                case opers[i].direction of
                  operOut:
                    if opers[i].namehi='' then
                      operline:=operline+GetOper(opers[i].typ);
                  operVar:
                    operline:=operline+GetOper(opers[i].typ);
                  operIn:
                    operline:=operline+GetOper(opers[i].typ);
                end;
              end;

            if operline='taicpu.op' then
              operline:='taicpu.op_none(op'
            else
              operline:=operline+'(op';

            cnt:=0;
            for i := 0 to opercnt-1 do
              begin
                case opers[i].direction of
                  operOut:
                    if opers[i].namehi='' then
                      operline:=operline+GetOperandLoc(opers[i].typ);
                  operIn,
                  operVar:
                    begin
                      operline:=operline+GetOperand(opers[i].typ, cnt+1);
                      inc(cnt);
                    end;
                end;
              end;

            operline:=operline+')';

            fsecond.Add('    current_asmdata.CurrAsmList.concat(setoppostfix('+operline+',pf));');

            fsecond.Add('  end;')
          end;

        // Write innr
        writeln(finnr, '  in_', APrefix,'_',instrPart,postfix+_alias,' = in_',APrefix,'_base+',intrnum,';');

        // Write function
        if hasOutput then write(fprocs,'function ') else write(fprocs,'procedure ');
        write(fprocs,APrefix,'_',instrPart,postfix,'(');

        cnt:=0;
        for i:=0 to opercnt-1 do
          begin
            if opers[i].direction=operOut then
              Continue;

            if cnt>0 then
              begin
                if opers[i].typ<>opers[i-1].typ then
                  write(fprocs,': ',GetPascalType(opers[i-1].typ),'; ')
                else
                  write(fprocs,', ');
              end;

            write(fprocs,opers[i].name);
            if i=opercnt-1 then
              write(fprocs,': ',GetPascalType(opers[i].typ));

            inc(cnt);
          end;

        write(fprocs,')');

        if hasOutput then write(fprocs,': ',GetPascalType(outputType));
        writeln(fprocs,'; [INTERNPROC: in_',APrefix,'_',instrPart,postfix+_alias,'];');

        // Str now contains conditionals

        inc(intrnum);
      end;

    writeln(finnr, '  in_', APrefix,'_last = in_',APrefix,'_base+',intrnum-1,';');

    ftypechk.Insert(0,format(headercomment,[AFilename]));
    ftypechk.SaveToFile(APrefix+'type.inc');

    ffirst.Insert(0,format(headercomment,[AFilename]));
    ffirst.SaveToFile(APrefix+'first.inc');

    fsecond.Insert(0,format(headercomment,[AFilename]));
    fsecond.SaveToFile(APrefix+'second.inc');

    ftypechk.Free;
    ffirst.Free;
    fsecond.Free;

    CloseFile(fprocs);
    CloseFile(finnr);

    closefile(f);
  end;

begin
  ParseList('arm', 'armintr.dat');
end.

