
var
  pw_log_level: Tspa_log_level; cvar;external;
  PW_LOG_TOPIC_DEFAULT: Pspa_log_topic; cvar;external;

procedure pw_log_set(log: Pspa_log); cdecl; external libpipewirename Name 'pw_log_set';

function pw_log_get : Pspa_log; cdecl; external libpipewirename Name 'pw_log_get';

procedure pw_log_set_level(level: Tspa_log_level); cdecl; external libpipewirename Name 'pw_log_set_level';

procedure pw_log_logt(level: Tspa_log_level; topic: Pspa_log_topic; file_: PChar; line: longint; func: PChar; fmt: PChar; args: array of const); cdecl; external libpipewirename Name 'pw_log_logt';

procedure pw_log_logt(level: Tspa_log_level; topic: Pspa_log_topic; file_: PChar; line: longint; func: PChar; fmt: PChar); cdecl; external libpipewirename Name 'pw_log_logt';

procedure pw_log_logtv(level: Tspa_log_level; topic: Pspa_log_topic; file_: PChar; line: longint; func: PChar; fmt: PChar; args: pointer); cdecl; external libpipewirename Name 'pw_log_logtv';

procedure pw_log_log(level: Tspa_log_level; file_: PChar; line: longint; func: PChar; fmt: PChar; args: array of const); cdecl; external libpipewirename Name 'pw_log_log';

procedure pw_log_log(level: Tspa_log_level; file_: PChar; line: longint; func: PChar; fmt: PChar); cdecl; external libpipewirename Name 'pw_log_log';

procedure pw_log_logv(level: Tspa_log_level; file_: PChar; line: longint; func: PChar; fmt: PChar; args: pointer); cdecl; external libpipewirename Name 'pw_log_logv';

procedure _pw_log_topic_new(topic: Pspa_log_topic); cdecl; external libpipewirename Name '_pw_log_topic_new';


