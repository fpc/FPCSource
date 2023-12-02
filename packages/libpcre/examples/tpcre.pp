program tpcre;

{$mode objfpc}
{$h+}
{ $DEFINE USE_WIDESTRING}

uses
   {$IFNDEF USE_WIDESTRING}
   libpcre2_8,
   {$ELSE}
   libpcre2_16,
   {$ENDIF}
   ctypes;


{$IFNDEF USE_WIDESTRING}
function GetStrLen(p : PAnsiChar; len : Integer) : AnsiString;

var
   L : Integer;

begin
  Result:='';
  L:=StrLen(P);
  if L>len then
    L:=Len;
  SetLength(Result,L);
  if Len>0 then
    Move(P^,Result[1],L);
end;
{$ELSE}
function GetStrLen(p : PWideChar; len : Integer) : UnicodeString;

var
  L : Integer;
  P2: PWideChar;

begin
  Result:='';
  L:=0;
  P2:=P;
  // No widestring strlen unless we compile in unicode rtl...
  While (P2^<>#0) do
    begin
    inc(L);
    inc(P2);
    end;
  if L>len then
    L:=Len;
  SetLength(Result,L);
  if Len>0 then
    Move(P^,Result[1],L*2);
end;
{$ENDIF}

var
 re : ppcre2_code;
 {$IFNDEF USE_WIDESTRING}
 ptrn : AnsiString;
 subj : AnsiString;
 groupname : AnsiString;
 {$ELSE}
 ptrn : UnicodeString;
 subj : UnicodeString;
 groupname : UnicodeString;
 {$ENDIF}

 pattern : PCRE2_SPTR;     (* PCRE2_SPTR is a pointer to unsigned code units of *)
 subject : PCRE2_SPTR;     (* the appropriate width (in this case, 8 bits). *)
 name_table : PCRE2_SPTR;

 utf8,
 crlf_is_newline,
 find_all : Boolean;
 errornumber,
 i,n,matchlen,
 rc : cint;

 newline,
 name_entry_size,
 namecount,
 options,
 option_bits : cuint32 ;

 erroroffset : PCRE2_SIZE;
 ovector : ^PCRE2_SIZE;
 pattern_length,
 subject_length : PCRE2_SIZE;

 match_data : ppcre2_match_data;
 buffer : Array[0..255] of ansichar;
 substring_start : PCRE2_SPTR;
 substring_length : PCRE2_SIZE;
 tabptr: PCRE2_SPTR ;
 start_offset : PCRE2_SIZE;
 startchar : PCRE2_SIZE ;

begin
  (*
    ***************************************************************************
    * First, sort out the command line. There is only one possible option at  *
    * the moment, "-g" to request repeated matching to find all occurrences,  *
    * like Perl's /g option. We set the variable find_all to a non-zero value *
    * if the -g option is present.                                            *
  ***************************************************************************
  *)

  find_all:=False;
  I:=1;
  While I<=ParamCount do
    begin
    if (ParamStr(i)='-g') then
      find_all:=True
    else if (ParamStr(i)[1] = '-') then
      begin
      Writeln('Unrecognised option: ', paramstr(i));
      halt(1);
      end
    else
      Break;
    Inc(I);
    end;

  (*
    After the options, we require exactly two arguments, which are the pattern,
    and the subject string.
  *)

  if ((ParamCount-i+1)<>2) then
    begin
    Writeln('Exactly two arguments required: a regex and a subject string');
    Halt(1);
    end;

  (*
    Pattern and subject are char arguments, so they can be straightforwardly
    cast to PCRE2_SPTR because we are working in 8-bit code units. The subject
    length is cast to PCRE2_SIZE for completeness, though PCRE2_SIZE is in fact
    defined to be size_t.
  *)

  ptrn:=ParamStr(I);
  subj:=ParamStr(I+1);
  {$IFNDEF USE_WIDESTRING}
  pattern:=PAnsiChar(ptrn);
  subject:=PAnsiChar(Subj);
  {$ELSE}
  pattern:=PUnicodeChar(ptrn);
  subject:=PUnicodeChar(Subj);
  {$ENDIF}
  pattern_length:=length(pattern);
  subject_length:=Length(subject);


  (*
    **************************************************************************
    * Now we are going to compile the regular expression pattern, and handle *
    * any errors that are detected.                                          *
    **************************************************************************
  *)

{$IFDEF USE_WIDESTRING}
  re:=pcre2_compile_w(
{$ELSE}
  re:=pcre2_compile(
{$ENDIF}
    pattern, (* the pattern *)
    pattern_length,        (* Pattern-length *)
    0,                     (* default options *)
    @errornumber,          (* for error number *)
    @erroroffset,          (* for error offset *)
    Nil);                 (* use default compile context *)

  (*
  Compilation failed: print the error message and exit.
  *)

  if (re=Nil) then
    begin
    pcre2_get_error_message(errornumber, @buffer, sizeof(buffer));
    Writeln('PCRE2 compilation failed at offset ',erroroffset,': ',buffer);
    halt(1)
    end;


  (*
    **************************************************************************
    * If the compilation succeeded, we call PCRE2 again, in order to do a    *
    * pattern match against the subject string. This does just ONE match. If *
    * further matching is needed, it will be done below. Before running the  *
    * match we must set up a match_data block for holding the result. Using  *
    * pcre2_match_data_create_from_pattern() ensures that the block is       *
    * exactly the right size for the number of capturing parentheses in the  *
    * pattern. If you need to know the actual size of a match_data block as  *
    * a number of bytes, you can find it like this:                          *
    *                                                                        *
    * PCRE2_SIZE match_data_size = pcre2_get_match_data_size(match_data);    *
    **************************************************************************
  *)

  match_data := pcre2_match_data_create_from_pattern(re, Nil);

  (*
    Now run the match.
  *)

  {$IFDEF USE_WIDESTRING}
  rc := pcre2_match_w(
  {$ELSE}
  rc := pcre2_match(
  {$ENDIF}
    re,                   (* the compiled pattern *)
    subject,              (* the subject string *)
    subject_length,       (* the length of the subject *)
    0,                    (* start at offset 0 in the subject *)
    0,                    (* default options *)
    match_data,           (* block for storing the result *)
    Nil);                (* use default match context *)

  (*
    Matching failed: handle error cases
  *)

  if (rc < 0) then
    begin
    Case rc of
      PCRE2_ERROR_NOMATCH: Writeln('No match');
    else
       Writeln('Matching error ', rc);
    end;
    pcre2_match_data_free(match_data);   (* Release memory used for the match *)
    pcre2_code_free(re);                 (*   data and the compiled pattern. *)
    Halt(1);
    end;

  (*
    Match succeeded. Get a pointer to the output vector, where string offsets
    are stored.
  *)

  ovector := pcre2_get_ovector_pointer(match_data);
  Writeln('Match succeeded at offset ', integer(ovector[0]));


  (*
    **************************************************************************
    * We have found the first match within the subject string. If the output *
    * vector wasn't big enough, say so. Then output any substrings that were *
    * captured.                                                              *
  **************************************************************************
  *)

  (*
    The output vector wasn't big enough. This should not happen, because we used
    pcre2_match_data_create_from_pattern() above.
  *)

  if (rc = 0) then
    Writeln('ovector was not big enough for all the captured substrings');

  (*
    Since release 10.38 PCRE2 has locked out the use of \K in lookaround
    assertions. However, there is an option to re-enable the old behaviour. If that
    is set, it is possible to run patterns such as /(?=.\K)/ that use \K in an
    assertion to set the start of a match later than its end. In this demonstration
    program, we show how to detect this case, but it shouldn't arise because the
    option is never set.
  *)

  if (ovector[0] > ovector[1]) then
    begin
    i:=integer(ovector[0] - ovector[1]);
    Writeln('\K was used in an assertion to set the match start after its end.',
            'From end to start the match was:', GetStrLen(subject+ovector[1],i));
    Writeln('Run abandoned');
    pcre2_match_data_free(match_data);
    pcre2_code_free(re);
    Halt(1);
    end;

  (*
    Show substrings stored in the output vector by number. Obviously, in a real
    application you might want to do things other than print them.
  *)

  for i:=0 to rc-1 do
    begin
    substring_start := subject + ovector[2*i];
    substring_length := ovector[2*i+1] - ovector[2*i];
    Writeln(i:2, ': ',GetStrLen(substring_start,substring_length));
    end ;


  (*
    ***************************************************************************
    * That concludes the basic part of this demonstration program. We have    *
    * compiled a pattern, and performed a single match. The code that follows *
    * shows first how to access named substrings, and then how to code for    *
    * repeated matches on the same subject.                                   *
    ***************************************************************************
  *)

  (*
    See if there are any named substrings, and if so, show them by name.
    First we have to extract the count of named parentheses from the pattern.
  *)

  pcre2_pattern_info(
    re,                   (* the compiled pattern *)
    PCRE2_INFO_NAMECOUNT, (* get the number of named substrings *)
    @namecount);          (* where to put the answer *)

  if (namecount = 0) then
    Writeln('No named substrings')
  else
    begin
    Writeln('Named substrings');

    (*
      Before we can access the substrings, we must extract the table for
      translating names to numbers, and the size of each entry in the table.
    *)

    pcre2_pattern_info(
      re,                       (* the compiled pattern *)
      PCRE2_INFO_NAMETABLE,     (* address of the table *)
      @name_table);             (* where to put the answer *)

    pcre2_pattern_info(
      re,                       (* the compiled pattern *)
      PCRE2_INFO_NAMEENTRYSIZE, (* size of each entry in the table *)
      @name_entry_size);        (* where to put the answer *)

    (*
      Now we can scan the table and, for each entry, print the number, the name,
      and the substring itself. In the 8-bit library the number is held in two
      bytes, most significant first.
    *)

    tabptr := name_table;
    for i:=0 to namecount-1 do
      begin
{$IFDEF USE_WIDESTRING}
      n:=ord(tabptr[0]);
      groupname:=GetStrLen((TabPtr+1),name_entry_size-2);
{$ELSE}
      n:=(ord(tabptr[0]) shl 8) or ord(tabptr[1]);
      groupname:=GetStrLen((tabptr + 2),name_entry_size - 3),
{$ENDIF}
      matchlen:=integer(ovector[2*n+1] - ovector[2*n]);

      writeln( '(',n,')', Groupname,' : ',
                          GetStrLen((subject + ovector[2*n]), Matchlen));
      inc(tabptr, name_entry_size);
      end ;
    end  ;


  (*
    **************************************************************************
    * If the '-g' option was given on the command line, we want to continue  *
    * to search for additional matches in the subject string, in a similar   *
    * way to the /g option in Perl. This turns out to be trickier than you   *
    * might think because of the possibility of matching an empty string.    *
    * What happens is as follows:                                            *
    *                                                                        *
    * If the previous match was NOT for an empty string, we can just start   *
    * the next match at the end of the previous one.                         *
    *                                                                        *
    * If the previous match WAS for an empty string, we can't do that, as it *
    * would lead to an infinite loop. Instead, a call of pcre2_match() is    *
    * made with the PCRE2_NOTEMPTY_ATSTART and PCRE2_ANCHORED flags set. The *
    * first of these tells PCRE2 that an empty string at the start of the    *
    * subject is not a valid match; other possibilities must be tried. The   *
    * second flag restricts PCRE2 to one match attempt at the initial string *
    * position. If this match succeeds, an alternative to the empty string   *
    * match has been found, and we can print it and proceed round the loop,  *
    * advancing by the length of whatever was found. If this match does not  *
    * succeed, we still stay in the loop, advancing by just one character.   *
    * In UTF-8 mode, which can be set by ( *UTF) in the pattern, this may be *
    * more than one byte.                                                    *
    *                                                                        *
    * However, there is a complication concerned with newlines. When the     *
    * newline convention is such that CRLF is a valid newline, we must       *
    * advance by two characters rather than one. The newline convention can  *
    * be set in the regex by ( *CR), etc.; if not, we must find the default. *
    **************************************************************************
  *)

  if Not find_all then    (* Check for -g *)
    begin
    pcre2_match_data_free(match_data);  (* Release the memory that was used *)
    pcre2_code_free(re);                (* for the match data and the pattern. *)
    Halt(0);                           (* Exit the program. *)
    end ;

  (*
    Before running the loop, check for UTF-8 and whether CRLF is a valid newline
    sequence. First, find the options with which the regex was compiled and extract
    the UTF state.
  *)

  pcre2_pattern_info(re, PCRE2_INFO_ALLOPTIONS, @option_bits);
  utf8 := ((option_bits and PCRE2_UTF) <> 0);

  (*
    Now find the newline convention and see whether CRLF is a valid newline
    sequence.
  *)

  pcre2_pattern_info(re, PCRE2_INFO_NEWLINE, @newline);
  crlf_is_newline :=  (newline = PCRE2_NEWLINE_ANY) or
                      (newline = PCRE2_NEWLINE_CRLF) or
                      (newline = PCRE2_NEWLINE_ANYCRLF);

(* Loop for second and subsequent matches *)

  While true do
    begin
    options := 0;                   (* Normally no options *)
    start_offset := ovector[1];   (* Start at end of previous match *)

    (*
      If the previous match was for an empty string, we are finished if we are
      at the end of the subject. Otherwise, arrange to run another match at the
      same point to see if a non-empty match can be found.
    *)

    if (ovector[0] = ovector[1]) then
      begin
      if (ovector[0] = subject_length) then
        break;
      options := PCRE2_NOTEMPTY_ATSTART or PCRE2_ANCHORED;
      end

    (*
      If the previous match was not an empty string, there is one tricky case to
      consider. If a pattern contains \K within a lookbehind assertion at the
      start, the end of the matched string can be at the offset where the match
      started. Without special action, this leads to a loop that keeps on matching
      the same substring. We must detect this case and arrange to move the start on
      by one character. The pcre2_get_startchar() function returns the starting
      offset that was passed to pcre2_match().
    *)

    else
      begin
      startchar := pcre2_get_startchar(match_data);
      if (start_offset <= startchar) then
        begin
        if (startchar >= subject_length) then
          break;                                 (* Reached end of subject.   *)
        start_offset:=startchar + 1;             (* Advance by one character. *)
        if utf8 then                        (* If UTF-8, it may be more  *)
          begin                                  (*   than one code unit.     *)
          While (start_offset < subject_length) do
            begin
            if ((Ord(subject[start_offset]) and $c0) <> $80) then
              break;
            inc(start_offset);
            end;
          end
        end
      end ;

    (*
      Run the next matching operation
    *)

    {$IFDEF USE_WIDESTRING}
    rc := pcre2_match_w(
    {$ELSE}
    rc := pcre2_match(
    {$ENDIF}
      re,                   (* the compiled pattern *)
      subject,              (* the subject string *)
      subject_length,       (* the length of the subject *)
      start_offset,         (* starting offset in the subject *)
      options,              (* options *)
      match_data,           (* block for storing the result *)
      Nil);                (* use default match context *)

    (*
      This time, a result of NOMATCH isn't an error. If the value in 'options'
      is zero, it just means we have found all possible matches, so the loop ends.
      Otherwise, it means we have failed to find a non-empty-string match at a
      point where there was a previous empty-string match. In this case, we do what
      Perl does: advance the matching position by one character, and continue. We
      do this by setting the 'end of previous match' offset, because that is picked
      up at the top of the loop as the point at which to start again.

      There are two complications: (a) When CRLF is a valid newline sequence, and
      the current position is just before it, advance by an extra byte. (b)
      Otherwise we must ensure that we skip an entire UTF character if we are in
      UTF mode.
    *)

    if (rc = PCRE2_ERROR_NOMATCH) then
      begin
      if (options = 0) then
        break;                                     (* All matches found *)
      ovector[1] := start_offset + 1;              (* Advance one code unit *)
      if (crlf_is_newline) and                  (* If CRLF is a newline & *)
          (start_offset < subject_length - 1) and  (* we are at CRLF, *)
          (subject[start_offset] = #13) and
          (subject[start_offset + 1] = #10) then
        inc(ovector[1])                             (* Advance by one more. *)
      else if (utf8) then                        (* Otherwise, ensure we *)
        begin                                       (* advance a whole UTF-8 *)
        while (ovector[1] < subject_length) do      (* character. *)
          begin
          if ((Ord(subject[ovector[1]]) and $c0) <> $80) then
            break;
          inc(ovector[1]);
          end;
        end;
      continue;    (* Go round the loop again *)
      end;

    (*
      Other matching errors are not recoverable.
    *)

    if (rc < 0) then
      begin
      Writeln('Matching error %d', rc);
      pcre2_match_data_free(match_data);
      pcre2_code_free(re);
      Halt(1);
      end;

    (*
      Match succeeded
    *)

    Writeln('Match succeeded again at offset ', integer (ovector[0]));

    (*
      The match succeeded, but the output vector wasn't big enough. This
      should not happen.
    *)

    if (rc = 0) then
      Writeln('ovector was not big enough for all the captured substrings');

    (*
      We must guard against patterns such as /(?=.\K)/ that use \K in an
      assertion to set the start of a match later than its end. In this
      demonstration program, we just detect this case and give up.
    *)

    if (ovector[0] > ovector[1]) then
      begin
      matchlen:=Integer(ovector[0] - ovector[1]);
      writeln('\K was used in an assertion to set the match start after its end.');
      Writeln('From end to start the match was: ', GetStrLen((subject + ovector[1]),matchlen));
      writeln('Run abandoned');
      pcre2_match_data_free(match_data);
      pcre2_code_free(re);
      Halt(1);
      end ;

    (*
      As before, show substrings stored in the output vector by number, and then
      also any named substrings.
    *)

    for i := 0 to rc-1 do
      begin
      substring_start:=subject + ovector[2*i];
      substring_length:=ovector[2*i+1] - ovector[2*i];
      Writeln(i,': ',GetStrLen(substring_start,substring_length));
      end;

    if (namecount = 0) then
      Writeln('No named substrings')
    else
      begin
      Writeln('Named substrings');
      for i:=0 to namecount-1 do
        begin
        {$IFDEF USE_WIDESTRING}
        n:=ord(tabptr[0]);
        groupname:=GetStrLen((TabPtr+1),name_entry_size-2);
        {$ELSE}
        n:=(ord(tabptr[0]) shl 8) or ord(tabptr[1]);
        groupname:=GetStrLen((tabptr + 2),name_entry_size - 3),
        {$ENDIF}
        matchlen:=integer(ovector[2*n+1] - ovector[2*n]);
        writeln( '(',n,')', Groupname,' : ', GetStrLen((subject + ovector[2*n]), Matchlen));
        tabptr := tabptr+name_entry_size;
        end ;
      end;
    end ;
  (*
    End of loop to find second and subsequent matches
  *)

  Writeln('');
  pcre2_match_data_free(match_data);
  pcre2_code_free(re);
end.

