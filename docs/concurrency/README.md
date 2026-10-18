# Конкурентність у FPC (гілка feature/concurrency)

Документація робіт з розвитку багатопоточності, асинхронного I/O та async/await
у цьому форку Free Pascal.

## Структура

| Файл / тека | Зміст |
|---|---|
| `concurrency-guide.pdf` | **Що нового**: огляд змін з короткими прикладами (генерується `build_pdf.py`) |
| `examples/` | Приклади з документації; кожен компілюється й запускається |
| `valgrind.md` | Перевірка гонок даних (helgrind/drd), анотації, відомі обмеження |
| `baseline.md` | Базова лінія тестів trunk до наших змін (відомі падіння) |
| `adr/` | Дизайн-рішення (Architecture Decision Records) |
| `guide-*.md` | Посібник користувача за темами |
| `../../CHANGELOG-concurrency.md` | Журнал змін гілки |

## Фази

0. Інфраструктура: збірка, baseline тестів, CI.
1. Стабілізація `System.Threading` (`packages/vcl-compat`).
2. Нові примітиви синхронізації (`packages/fcl-base/src/syncobjs.pp`).
3. Асинхронний I/O: пакет `packages/fcl-eventloop`.
4. async/await: спершу бібліотечно (продовження, корутини), потім, можливо, у компіляторі.

## Definition of Done

Кожна зміна містить: код, fpcunit-тести, стрес-тест (якщо є паралелізм),
fpdoc-опис API, приклад у `examples/`, запис у `CHANGELOG-concurrency.md`.

## Збірка

```sh
make clean all PP=<шлях до ppcx64 3.2.2> FPMAKEOPT="-T 12" -j12
```

**Важливо:** шлях до дерева джерел не повинен містити пробілів — makefile-и FPC
ламаються з помилкою `Only one source file supported`. Тому робоча копія гілки —
git worktree `/home/chessplayer/DEV/Pascal/fpc-concurrency`.

### Встановлення trunk у домашню теку (потрібно для `tests/`)

Тестовий набір компілятора очікує «встановлений» FPC з `fpc.cfg`:

```sh
make install INSTALL_PREFIX=$HOME/fpc-trunk PP=$PWD/compiler/ppcx64
mkdir -p ~/fpc-trunk/etc
~/fpc-trunk/bin/fpcmkcfg -d basepath=$HOME/fpc-trunk/lib/fpc/3.3.1 -o ~/fpc-trunk/etc/fpc.cfg
export PPC_CONFIG_PATH=$HOME/fpc-trunk/etc
export PATH=$HOME/fpc-trunk/bin:$PATH   # потрібен fpcmake
cd tests && make full TEST_FPC=$HOME/fpc-trunk/lib/fpc/3.3.1/ppcx64 \
                      FPC=$HOME/fpc-trunk/lib/fpc/3.3.1/ppcx64 -j12
```

Bootstrap-компілятор 3.2.2 розпаковано без root з пакета Ubuntu
(`apt-get download fp-compiler-3.2.2` + `dpkg -x`) у `~/fpc-bootstrap/root/`.

## Документація PDF

```sh
python3 -m venv /tmp/venv && /tmp/venv/bin/pip install reportlab
/tmp/venv/bin/python docs/concurrency/build_pdf.py
```

Приклади читаються безпосередньо з `examples/*.pp`; після зміни прикладу
перевірте його (`fpc` з `tests/concurrency/fpcenv.sh`) і перегенеруйте PDF.
