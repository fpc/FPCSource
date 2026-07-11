{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Resource strings: the messages of the analysis core and CLI

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpSonar.Consts;

{$mode objfpc}{$H+}

interface

resourcestring
  // Product banner (FpSonar.Types.FpSonarBanner)
  SBanner = 'Fp-Sonar %s - LCL-free Object Pascal static analyzer';

  // Reserved diagnostic catalog templates (folded parse/scan/rule/resolve issues)
  SParseError = 'Parse error: %s';
  SScanError = 'Scan error: %s';
  SFileNotFound = 'Cannot open source file: %s';
  SRuleError = 'Rule "%s" failed: %s';
  SResolveError = 'Resolution failed: %s';

  // Quality-gate failure reasons (FpSonar.Config: TFpSonarGateThresholds.Evaluate)
  SGateSeverityExceeded = '%d %s issue(s) exceed the configured maximum of %d';
  SGateTotalExceeded = '%d total issue(s) exceed the configured maximum of %d';

  // Command-line usage errors (FpSonar.CLI.Options)
  SMissingValue = 'missing value for %s';
  SUnknownFormat = 'unknown format: %s';
  SUnknownDialect = 'unknown dialect: %s (expected: default, pas2js)';
  SCannotLoadConfig = 'cannot load config "%s": %s';
  SUnknownOption = 'unknown option: %s';
  SMutuallyExclusive = '--new-code and --baseline are mutually exclusive';

  // Command-line diagnostics + report footer (fpsonar.lpr)
  SCliError = 'fpsonar: %s';
  SCannotWriteOutput = 'fpsonar: cannot write output file: %s';
  STryHelp = 'Try "fpsonar --help" for usage.';
  SProjectLoadNote = 'note: could not load project file: %s';
  SGateFailedNote = 'note: quality gate failed: %s';
  SAnalyzedSummary = 'Analyzed %d file(s): %d issue(s).';

  // Command-line usage / help text (FpSonar.CLI.Options.HelpText)
  SHelpText =
    'usage: fpsonar analyze [options] <path...>'#10 +
    '       fpsonar baseline [options] [-o <file>] <path...>'#10 +
    '       fpsonar init-config [-o <file>]'#10 +
    ''#10 +
    'Analyze one or more Object Pascal files, directories, or projects.'#10 +
    ''#10 +
    'Commands:'#10 +
    '  analyze           report issues (default flow). With --new-code <file>'#10 +
    '                    report+gate ONLY issues absent from that baseline.'#10 +
    '  baseline          write a snapshot of the current effective issue'#10 +
    '                    fingerprints to --output/-o (default: stdout); no'#10 +
    '                    quality gate.'#10 +
    '  init-config       write a complete JSON config listing every rule with'#10 +
    '                    its default enabled state + severity, plus the gate'#10 +
    '                    policy, to --output/-o (default: stdout). Edit it and'#10 +
    '                    pass it back with --config/-c.'#10 +
    ''#10 +
    'Targets:'#10 +
    '  <path>            a .pas/.pp/.lpr file, a directory (expanded'#10 +
    '                    recursively), or a .lpi/.lpk project (loaded).'#10 +
    ''#10 +
    'Options:'#10 +
    '  --define <D>, -d<D>   add a conditional define'#10 +
    '  --mode <M>, -M<M>     set the compiler mode (e.g. OBJFPC, DELPHI)'#10 +
    '  -Fu<path>            add a unit search path'#10 +
    '  -Fi<path>            add an include search path'#10 +
    '  --cpu <C>            set the target CPU (e.g. x86_64)'#10 +
    '  --os <O>             set the target OS (e.g. linux)'#10 +
    '  --project <f>, -p <f>  load a .lpi/.lpk/fpc.cfg project file'#10 +
    '  --format <fmt>, -f <fmt>  report format: text (default), sarif, sonar-json'#10 +
    '  --output <f>, -o <f>  write the report to a file (default: stdout)'#10 +
    '  --config <f>, -c <f>  load a JSON config (rules + quality-gate thresholds)'#10 +
    '  --new-code <f>       analyze: report+gate only issues absent from baseline <f>'#10 +
    '  --baseline <f>       analyze: report ALL issues but mute (exclude from the'#10 +
    '                       gate) those already in baseline <f>; muted issues are'#10 +
    '                       annotated with their suppression source. Mutually'#10 +
    '                       exclusive with --new-code.'#10 +
    '  --real-rtl           resolve the RTL chain (System/SysUtils/Classes) against'#10 +
    '                       the real FPC source first (synthetic fallback); uses'#10 +
    '                       --cpu/--os/-Fu/-Fi overrides. Default: synthetic stubs.'#10 +
    '  --synthetic-only     resolve every `uses` against the host-independent'#10 +
    '                       synthetic RTL only. Opts OUT of the DEFAULT auto-detect,'#10 +
    '                       which generates a faithful interface stub on demand from'#10 +
    '                       your OWN version-matched .ppu via `ppudump` (cached) and'#10 +
    '                       falls back to synthetic. Use for deterministic,'#10 +
    '                       FPC-version-independent runs (CI/self-lint/no-FPC host).'#10 +
    '  --ppu-cache <dir>    keep the on-demand ppudump stubs in <dir> PERMANENTLY'#10 +
    '                       (default: per-run temp files, discarded at exit). A warm'#10 +
    '                       cache resolves without re-running ppudump; entries are'#10 +
    '                       keyed by each .ppu''s size+mtime and refreshed when it'#10 +
    '                       changes. No effect under --synthetic-only.'#10 +
    '  --dialect <name>     source dialect: default (FPC) or pas2js. pas2js enables'#10 +
    '                       pas2js-only syntax (async, external class, ...) and'#10 +
    '                       parses the RTL from real source (no .ppu).'#10 +
    '  --pas2js             shorthand for --dialect pas2js.'#10 +
    '  --pas2js-src <dir>   add <dir> to the unit search path for pas2js real-source'#10 +
    '                       resolution (repeatable; e.g. the pas2js packages/rtl).'#10 +
    '  --help, -h           show this help'#10 +
    ''#10 +
    'The -Fu/-Fi/-d/-M flags accept both glued (-FuX) and separated (-Fu X) forms.'#10 +
    'The quality gate exits 1 when configured thresholds are exceeded (2 on usage errors).'#10 +
    'Findings are silenced by inline // NOSONAR comments and the config''s "suppressions" globs.';


implementation

end.
