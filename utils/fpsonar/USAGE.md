# Using Fp-Sonar

Fp-Sonar is a **static analyzer** for Object Pascal (Free Pascal / Lazarus),
also known as a linter. "Static" means it reads your source code and points out problems 
 **without running it** — things like confusing names, risky patterns, or style that
doesn't match your team's conventions.

You give it some code, and it gives you back a list of issues. 
That's all there is to it. This guide walks you through it from zero.

---

## 1. The program

After building the project, the program lives here:

```
bin/x86_64-linux/fpsonar
```

In the examples below we just write `fpsonar`. (copy it to somewhere in your PATH)

To see the built-in help at any time:

```sh
fpsonar --help
```

---

## 2. Your first analysis

Point it at a single file:

```sh
fpsonar analyze myunit.pas
```

The word `analyze` is the **command** (what you want to do), and `myunit.pas`
is the **target** (what you want it to look at). That's the basic form of every command:

```
fpsonar <command> [options] <target...>
```

Here's what a run with no problems looks like:

```
Fp-Sonar 0.1.0 - text report v1
0 issue(s).
Analyzed 1 file(s): 0 issue(s).
```

And here's a run that found something:

```
Fp-Sonar 0.1.0 - text report v1
myunit.pas:42:13: minor LowercaseKeywords: Keyword should be lowercase: write True
Analyzed 1 file(s): 1 issue(s).
```

---

## 3. Reading the output

Each issue is one line, and it always has the same format:

```
myunit.pas : 42 : 13 : minor : LowercaseKeywords : Keyword should be lowercase
   file       line  col  severity   rule name          explanation
```

- **file** — which file has the issue.
- **line / col** — exactly where it is, so you can jump straight to it.
- **severity** — how serious it is (see the next section).
- **rule name** — the name of the check that fired. Useful when you want to
  turn a check off or read up on it.
- **explanation** — a short description of what's wrong.

At the bottom you always get a one-line summary: how many files were looked at
and how many issues were found.

---

## 4. Severities and the "pass / fail" result

Every issue has a **severity**, from most to least serious:

| Severity   | Meaning                                    |
|------------|--------------------------------------------|
| `blocker`  | Almost certainly a real bug — fix it.      |
| `critical` | Very likely a problem.                     |
| `major`    | Worth fixing.                              |
| `minor`    | Small style / polish issue.                |
| `info`     | Just information; not a problem.           |

Fp-Sonar also gives your whole run a **pass or fail** result, called the
**quality gate**. You find out the result from the program's **exit code** — a
number the program returns that scripts (and CI systems) can check:

| Exit code | What it means                                             |
|-----------|-----------------------------------------------------------|
| `0`       | Success — the gate passed.                                |
| `1`       | The gate **failed** (too many serious issues).            |
| `2`       | You made a mistake in the command (e.g. a typo in a flag).|

By default the gate **fails only if there are any `blocker` or `critical`
issues**. Minor style issues are reported but won't fail the gate. 
You can change these limits with a config file (section 8).

To see the exit code in a terminal, run this right after `fpsonar`:

```sh
echo $?
```

---

## 5. What can you point it at?

The target can be:

- **A single file** — `fpsonar analyze myunit.pas`
- **A folder** — it looks at every `.pas`, `.pp`, and `.lpr` file inside,
  including sub-folders:

  ```sh
  fpsonar analyze src/
  ```

- **A Lazarus project** (`.lpi`) or package (`.lpk`) — it reads the project's
  settings for you and analyzes its units:

  ```sh
  fpsonar analyze myproject.lpi
  ```

You can also list several targets at once:

```sh
fpsonar analyze unita.pas unitb.pas src/extra/
```

---

## 6. Saving the report and choosing a format

By default the report prints to your screen. To save it to a file instead:

```sh
fpsonar analyze src/ --output report.txt
```

Fp-Sonar can produce three formats with `--format`:

- `text` — the human-friendly default you've been seeing.
- `sarif` — a standard JSON format many editors and code-review tools read.
- `sonar-json` — the format for feeding results into SonarQube.

```sh
fpsonar analyze src/ --format sarif --output report.sarif
```

---

## 7. Silencing one specific line

Sometimes a rule flags a line that you know is fine. Add a `// NOSONAR` comment
to that line and Fp-Sonar will ignore it:

```pascal
x := SomethingWeird();  // NOSONAR - this is intentional, see ticket #123
```

Use this sparingly — it's an override, so anyone reading the code later will
trust that you meant it.

---

## 8. Turning rules on/off and setting limits (config file)

To change *which* rules run or *when the gate fails*, you write a small **config
file** in JSON and pass it with `--config`:

```sh
fpsonar analyze src/ --config myrules.json
```

A config file looks like this:

```json
{
  "rules": {
    "LowercaseKeywords": { "enabled": false },
    "ConstantNaming":    { "params": { "pattern": "^c?[A-Z][A-Za-z0-9_]*$" } }
  },
  "gate": {
    "maxBlocker":  0,
    "maxCritical": 0,
    "maxMajor":   10
  }
}
```

What this does:

- **`rules`** — adjust individual checks.
  - `"enabled": false` turns a rule off completely.
  - `"params"` tunes a rule. In the example, we tell the naming rule what a
    valid constant name looks like (a "pattern"). A value of `-1` anywhere in
    the gate means "no limit".
- **`gate`** — the maximum number of issues of each severity you'll tolerate
  before the run **fails** (exit code `1`). `maxMajor: 10` means "fail if there
  are more than 10 major issues."

You don't have to configure anything — without `--config`, sensible built-in
defaults are used (all rules on; fail only on blocker/critical).

> There are ready-made examples in the `config/` folder you can copy and edit.

### Generating a starting config

Rather than writing the file from scratch, let fpsonar emit one with **every rule** 
already listed at its default enabled state and severity — and, for
rules that have tunable parameters, a `params` block showing each one at its
built-in default:

```sh
fpsonar init-config -o myrules.json
```

(Without `-o` it prints to the screen, so you can pipe or redirect it yourself.)
Open the file, mark the rules you don't want with `"enabled": false`, adjust any
severities or gate limits, then pass it back with `--config`:

```sh
fpsonar analyze src/ --config myrules.json
```

### Silencing whole rules or paths (suppressions)

The `// NOSONAR` comment (section 7) silences *one line*. When you want to
silence findings in bulk — a whole rule, a whole folder, or a rule within a
folder — add a `suppressions` list to the config file instead:

```json
{
  "suppressions": [
    { "rule": "Naming*" },
    { "path": "*/legacy/*" },
    { "rule": "LineTooLong", "path": "*/generated/*.pas" }
  ]
}
```

Each entry has an optional `rule` and an optional `path` (you must give at
least one). A finding is silenced when **both** patterns match it: its rule
name matches `rule` **and** its file path matches `path`. A field you leave
out means "match anything", so the three entries above read as:

- silence every rule whose name starts with `Naming`, anywhere;
- silence *all* rules under any `legacy/` folder;
- silence `LineTooLong` only in `.pas` files under a `generated/` folder.

The patterns are **globs**, matched **case-sensitively**:

- `*` matches any run of characters, including `/` (so it spans folders);
- `?` matches exactly one character;
- every other character matches itself.

**Suppression vs. disabling a rule.** Turning a rule off with
`"enabled": false` and suppressing it are *not* the same thing:

- **Disabling** stops the rule from running at all — it produces no findings,
  anywhere, and can't be scoped. Use it when a rule simply doesn't apply to
  your project.
- **Suppressing** lets the rule keep running and then hides only the findings
  that match your `rule`/`path` globs. Use it when a rule is worth having in
  general but you want to exempt certain files or folders.

So a suppression can keep a rule on for the whole project *except* where it's
noise (generated code, third-party sources, a legacy corner) — something
disabling can't do. And because the rule still runs, suppressed findings are
not lost: under `--baseline` they are still reported, annotated with their
suppression source, rather than vanishing.

Unlike `// NOSONAR`, suppressions live in one central file, so use them for
policy decisions ("we don't lint generated code") rather than one-off
exceptions.

---

## 9. Analyzing a real project (compiler settings)

Real code often depends on **compiler settings** to make sense — a compiler
mode, conditional `{$IFDEF}` defines, and folders where other units live. If a
plain `analyze` reports lots of "cannot find unit" style noise, give Fp-Sonar
the same settings your compiler uses:

```sh
fpsonar analyze src/ \
  -Mobjfpc \
  -dDEBUG \
  -Fu/path/to/other/units \
  -Fi/path/to/include/files
```

- `-M<mode>` — the compiler mode (e.g. `-Mobjfpc` or `-Mdelphi`).
- `-d<NAME>` — define a symbol, like the compiler's `-d`. Use it once per symbol.
- `-Fu<path>` — a folder to look in for **units** your code `uses`.
- `-Fi<path>` — a folder to look in for **include** (`{$I ...}`) files.

**Easiest option:** if you have a Lazarus `.lpi`, just analyze that
(`fpsonar analyze myproject.lpi`) and these settings are taken from the project
automatically.

> Tip: these four flags accept both `-Fu/some/path` (glued) and `-Fu /some/path`
> (with a space) — whichever you prefer.

---

## 10. Working with an existing (large) codebase

If you switch on Fp-Sonar on a big project, you probably don't want to fix
thousands of old issues at once. The idea is: **freeze today's issues, then only
worry about new ones.**

**Step 1 — take a snapshot** (a "baseline") of everything as it is today:

```sh
fpsonar baseline src/ --output baseline.json
```

**Step 2 — from now on, only report issues that are *new* since the snapshot:**

```sh
fpsonar analyze src/ --new-code baseline.json
```

Now the gate only cares about problems you've *added*, so you or your team can improve
things gradually. (A related flag, `--baseline`, still shows the old issues but
doesn't let them fail the gate — handy if you'd rather see everything.)

---

## 11. Where does it look up standard units? (advanced, optional)

To understand code that uses standard units (`SysUtils`, `Classes`, …),
Fp-Sonar needs to know what's inside them. It figures this out **automatically**
using your installed Free Pascal — you don't have to do anything.

You only need this flag in special cases:

- `--synthetic-only` — don't use your installed FPC at all; rely on built-in
  approximations of the standard units. Use this for **repeatable results that
  don't depend on which FPC version is installed** (for example on a build
  server, or a machine without FPC). It's slightly less precise but always
  behaves the same everywhere.

There's also `--real-rtl` for preferring the real FPC *source*; most people
never need it.

### Why there's a cache, and where it lives

To learn what's inside a standard unit, Fp-Sonar asks your Free Pascal toolchain
to describe it — it runs the `ppudump` program on the compiled unit files
(`.ppu`) that ship with your FPC, and turns the answer into a small, temporary
"summary" of that unit. Doing this takes a moment, so Fp-Sonar **saves each
summary and reuses it** instead of asking again for every file it analyzes.
That saved copy is the **ppudump cache**.

- **Where (default):** the summaries are written as temporary files in your
  system's temporary directory — usually `/tmp` (or whatever `$TMPDIR` points
  to). Their names start with `fpsppu`, so you may briefly see files like
  `/tmp/fpsppuXXXXX.tmp` while Fp-Sonar is running.
- **How long they last (default):** the cache lives only for **one run**. Each
  unit is summarized at most once per run, and **all the temporary files are
  deleted automatically when Fp-Sonar exits.** Nothing is left behind between
  runs, and nothing is stored in your project.
- **You don't have to manage it:** there's no cache to clear and no setting to
  turn it on — it just makes a single run faster. If you'd rather not touch your
  installed FPC at all, `--synthetic-only` skips this whole mechanism entirely.

#### Making the cache permanent (`--ppu-cache`)

By default each run rebuilds these summaries from scratch. If you run Fp-Sonar
often (say, on every save or in a fast CI loop), you can tell it to **keep the
summaries in a folder and reuse them next time**:

```sh
fpsonar analyze src/ --ppu-cache .fpsonar-cache
```

The first run fills the folder; later runs read from it and **skip `ppudump`
entirely**, so they start faster. Fp-Sonar keeps the cache correct on its own:

- Each summary is tagged with the size and modification time of the `.ppu` it
  came from. If that file changes (for example, you upgrade Free Pascal), the
  tag no longer matches and Fp-Sonar rebuilds the summary automatically.
- In practice the cached units are the standard-library `.ppu` that ship with
  your FPC and never change on their own — so the cache stays valid for a long
  time and there's normally nothing to clean up.
- If anything about the folder can't be used (missing, read-only, …), Fp-Sonar
  silently falls back to the per-run temporary files — your analysis is never
  affected.
- The flag has no effect together with `--synthetic-only` (which turns the
  whole mechanism off).

To clear it, just delete the folder. It's safe to point several projects at the
same cache folder.

---

## 12. Quick reference

**Commands**

| Command    | What it does                                                      |
|------------|-------------------------------------------------------------------|
| `analyze`  | Look at code and report issues (the normal command).              |
| `baseline` | Write a snapshot of current issues to a file (no pass/fail check). |

**Common options**

| Option                    | What it does                                        |
|---------------------------|-----------------------------------------------------|
| `--output <file>`, `-o`   | Write the report to a file instead of the screen.   |
| `--format <fmt>`, `-f`    | `text` (default), `sarif`, or `sonar-json`.         |
| `--config <file>`, `-c`   | Use a JSON config (which rules run + gate limits).  |
| `--project <file>`, `-p`  | Load a `.lpi` / `.lpk` project's settings.          |
| `--new-code <file>`       | Only report issues **not** in that baseline file.   |
| `--baseline <file>`       | Show all issues but don't let old ones fail the gate.|
| `--ppu-cache <dir>`       | Keep the standard-unit summaries in `<dir>` and reuse them across runs (faster startup). |
| `--synthetic-only`        | Don't use your installed FPC; use built-in approximations (repeatable everywhere). |
| `--dialect <name>`        | `default` (FPC) or `pas2js`. `pas2js` accepts pas2js-only syntax (async, external class, …) and resolves the RTL from real source (pas2js has no `.ppu`). |
| `--pas2js`                | Shorthand for `--dialect pas2js`.                   |
| `--pas2js-src <dir>`      | Add `<dir>` to the unit search path for pas2js real-source resolution (repeatable, e.g. the pas2js `packages/rtl`). |
| `--help`, `-h`            | Show the built-in help.                             |

**Compiler-setting options** (for real projects)

| Option           | What it does                              |
|------------------|-------------------------------------------|
| `-M<mode>`       | Compiler mode (e.g. `objfpc`, `delphi`).  |
| `-d<NAME>`       | Define a symbol (like `{$DEFINE NAME}`).  |
| `-Fu<path>`      | Folder to find `uses` units in.           |
| `-Fi<path>`      | Folder to find `{$I}` include files in.   |
| `--cpu <c>`      | Target CPU (e.g. `x86_64`).               |
| `--os <o>`       | Target OS (e.g. `linux`).                 |

**Exit codes**

| Code | Meaning                              |
|------|--------------------------------------|
| `0`  | Passed.                              |
| `1`  | Quality gate failed.                 |
| `2`  | Something wrong with your command.   |

---

## 13. Common questions

* **"What's with the name fpSonar?"**
   The tool was born out of frustration with SonarQube. A 1000-headed dragon written in Java
   that requires several gigabytes of RAM and 32Gb of disk space for elasticsearch or somesuch.
   If that sounds like overkill to you, fpSonar is for you.
* **"It found nothing — is that right?"**
   Maybe! A clean file really does report `0 issue(s)`. Try running it on a folder
   with more code to see it in action.
* **"I get lots of 'cannot resolve' style noise on a real project."**
   Give it the compiler settings (section 9), or point it at your `.lpi`
   (section 5). That usually clears it up.
* **"How do I make one annoying warning go away?"**
   For a single line, use `// NOSONAR` (section 7). To turn a rule off everywhere,
  disable it in a config file (section 8).
* **"How do I use this in CI (automated builds)?"**
   Run `fpsonar analyze ...` as a build step and let the **exit code** decide
   pass/fail — most CI systems fail the build automatically when a step returns a
   non-zero code. Combine with `--new-code` (section 10) so only newly introduced
   issues can fail the build.
