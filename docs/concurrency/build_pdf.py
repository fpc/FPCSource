#!/usr/bin/env python3
"""Генерує docs/concurrency/concurrency-guide.pdf.

Приклади коду читаються з docs/concurrency/examples/*.pp, тож PDF завжди
збігається з кодом, який компілюється й запускається.
Потрібен reportlab і шрифти DejaVu (кирилиця).
Використання: python3 docs/concurrency/build_pdf.py
"""
import os
from xml.sax.saxutils import escape

from reportlab.lib import colors
from reportlab.lib.enums import TA_LEFT
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import ParagraphStyle
from reportlab.lib.units import mm
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.platypus import (KeepTogether, PageBreak, Paragraph, SimpleDocTemplate,
                                Spacer, Table, TableStyle, XPreformatted)

HERE = os.path.dirname(os.path.abspath(__file__))
EXAMPLES = os.path.join(HERE, "examples")
OUT = os.path.join(HERE, "concurrency-guide.pdf")

FONT_DIR = "/usr/share/fonts/truetype/dejavu"
pdfmetrics.registerFont(TTFont("Sans", os.path.join(FONT_DIR, "DejaVuSans.ttf")))
pdfmetrics.registerFont(TTFont("Sans-Bold", os.path.join(FONT_DIR, "DejaVuSans-Bold.ttf")))
pdfmetrics.registerFont(TTFont("Mono", os.path.join(FONT_DIR, "DejaVuSansMono.ttf")))
pdfmetrics.registerFontFamily("Sans", normal="Sans", bold="Sans-Bold",
                              italic="Sans", boldItalic="Sans-Bold")

INK = colors.HexColor("#1f2328")
MUTED = colors.HexColor("#57606a")
ACCENT = colors.HexColor("#0b5cad")
CODE_BG = colors.HexColor("#f4f6f8")
RULE = colors.HexColor("#d0d7de")

S_TITLE = ParagraphStyle("title", fontName="Sans-Bold", fontSize=20, leading=25,
                         textColor=INK, spaceAfter=4)
S_SUB = ParagraphStyle("sub", fontName="Sans", fontSize=10, leading=14,
                       textColor=MUTED, spaceAfter=14)
S_H1 = ParagraphStyle("h1", fontName="Sans-Bold", fontSize=14, leading=18,
                      textColor=ACCENT, spaceBefore=14, spaceAfter=6)
S_H2 = ParagraphStyle("h2", fontName="Sans-Bold", fontSize=11, leading=14,
                      textColor=INK, spaceBefore=8, spaceAfter=4)
S_BODY = ParagraphStyle("body", fontName="Sans", fontSize=9.5, leading=13.5,
                        textColor=INK, alignment=TA_LEFT, spaceAfter=5)
S_NOTE = ParagraphStyle("note", parent=S_BODY, textColor=MUTED, fontSize=8.5,
                        leading=12)
S_CELL = ParagraphStyle("cell", fontName="Sans", fontSize=8.5, leading=11.5,
                        textColor=INK)
S_CELL_B = ParagraphStyle("cellb", parent=S_CELL, fontName="Sans-Bold")
S_CODE = ParagraphStyle("code", fontName="Mono", fontSize=7.8, leading=10,
                        textColor=INK, backColor=CODE_BG, borderColor=RULE,
                        borderWidth=0.5, borderPadding=(5, 6, 5, 6),
                        leftIndent=6, rightIndent=6, spaceBefore=6, spaceAfter=6)
S_OUT = ParagraphStyle("out", parent=S_CODE, textColor=colors.HexColor("#1a7f37"))
S_CAPTION = ParagraphStyle("caption", parent=S_NOTE, keepWithNext=True,
                           spaceAfter=7, spaceBefore=2)


def c(text):
    """Inline code inside a paragraph."""
    return f'<font name="Mono" size="8.5">{escape(text)}</font>'


def p(text, style=S_BODY):
    return Paragraph(text, style)


class CodeBlock(XPreformatted):
    """Code listing: keeps line breaks, draws background, splits across pages."""

    def __init__(self, text, style):
        super().__init__(escape(text), style)


def code_block(text, style=S_CODE, title=None):
    """Caption (kept with the code) + a code block that may split across pages."""
    pre = CodeBlock(text.rstrip("\n"), style)
    if not title:
        return pre
    caption = Paragraph(escape(title), S_CAPTION)  # keepWithNext: stays with the code
    return KeepTogether([caption, pre]) if text.count("\n") < 12 else _Seq([caption, pre])


class _Seq(list):
    """A list of flowables returned as one item; flattened by flat()."""


def flat(items):
    out = []
    for it in items:
        if isinstance(it, _Seq):
            out.extend(it)
        else:
            out.append(it)
    return out


def example(name, output):
    with open(os.path.join(EXAMPLES, name), encoding="utf-8") as f:
        src = f.read()
    return flat([
        code_block(src, title=f"docs/concurrency/examples/{name}"),
        code_block(output, style=S_OUT, title="Вивід програми:"),
        Spacer(1, 4),
    ])


def table(rows, widths, header=True):
    data = []
    for i, row in enumerate(rows):
        style = S_CELL_B if (header and i == 0) else S_CELL
        data.append([Paragraph(cell, style) for cell in row])
    t = Table(data, colWidths=[w * mm for w in widths], repeatRows=1 if header else 0)
    st = [
        ("GRID", (0, 0), (-1, -1), 0.4, RULE),
        ("VALIGN", (0, 0), (-1, -1), "TOP"),
        ("LEFTPADDING", (0, 0), (-1, -1), 5),
        ("RIGHTPADDING", (0, 0), (-1, -1), 5),
        ("TOPPADDING", (0, 0), (-1, -1), 3),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 3),
    ]
    if header:
        st.append(("BACKGROUND", (0, 0), (-1, 0), CODE_BG))
    t.setStyle(TableStyle(st))
    return t


def footer(canvas, doc):
    canvas.saveState()
    canvas.setFont("Sans", 7.5)
    canvas.setFillColor(MUTED)
    canvas.drawString(20 * mm, 12 * mm, "FPC, гілка feature/concurrency — нове в багатопоточності")
    canvas.drawRightString(190 * mm, 12 * mm, f"{doc.page}")
    canvas.restoreState()


def build():
    s = []
    s.append(p("Багатопоточність у Free Pascal: що нового", S_TITLE))
    s.append(p("Гілка <b>feature/concurrency</b> (FPC 3.3.1) · жовтень 2026 · "
               "для розробників, які використовують System.Threading", S_SUB))

    s.append(p("Огляд", S_H1))
    s.append(p("Гілка виправляє помилки в " + c("System.Threading") + " (пакет vcl-compat), "
               "додає підтримку перевірки гонок даних через valgrind і робить тести "
               "багатопоточності надійнішими. Публічний API не змінено: існуючий код "
               "компілюється без змін і починає працювати коректніше."))
    s.append(table([
        ["Що", "Було", "Стало"],
        [c("TParallel.For"), "Іноді виконувався в одному потоці (20,7 с замість 1,7 с); "
         "ітерації могли губитися або виконуватися двічі", "Стабільно паралельний; кожна "
         "ітерація рівно один раз"],
        [c("TTask.Wait") + ", " + c("WaitForAny"), "Читання стану без бар'єра / без "
         "блокування", "Коректно і на x86, і на ARM64"],
        [c("TThreadPool.Free"), "Повертав керування, поки потоки пулу ще працювали; "
         "запис у звільнену пам'ять монітором", "Дочікується фактичного завершення "
         "всіх потоків"],
        ["valgrind / helgrind", "Сотні хибних попереджень; падіння valgrind", "Модуль "
         + c("fpvalgrind") + ", анотації, скрипти; 0 попереджень у System.Threading"],
        ["Тести", c("Assert") + " з робочих потоків міг загубити невдачу",
         c("ThreadCheck") + " / " + c("ThreadCheckSame")],
    ], [38, 62, 70]))

    s.append(p("1. Паралельний цикл TParallel.For", S_H1))
    s.append(p("Виправлено дві помилки. Перша: на щойно створеному («холодному») пулі "
               "вільні потоки не помічали роботу в локальній черзі кореневої задачі й "
               "спали до 40 с, тож увесь цикл виконувався одним потоком. Друга: потік "
               "забирав діапазон ітерацій з одним розміром кроку, а виконував з іншим, "
               "тому частина ітерацій губилася або виконувалася двічі, а "
               + c("TLoopResult.Completed") + " при цьому був " + c("True") + "."))
    s.extend(example("parallel_for.pp",
                     "Completed: TRUE, iterations not run exactly once: 0"))

    s.append(p("2. Задачі: TTask, IFuture, WaitForAll/WaitForAny", S_H1))
    s.append(p(c("Wait") + " перевіряє завершення без блокування (швидкий шлях); тепер це "
               "читання має бар'єр пам'яті, тож результат задачі видно одразу, і на "
               "процесорах зі слабким порядком пам'яті (ARM64) також. "
               + c("WaitForAny") + " читає індекс завершеної задачі під тим самим "
               "блокуванням, під яким його записують."))
    s.extend(example("tasks.pp",
                     "Future value: 42\nFirst finished: task #1\nAll finished: TRUE"))

    s.append(p("3. Власний пул потоків і його знищення", S_H1))
    s.append(p("Потоки пулу тепер належать пулу (" + c("FreeOnTerminate := False") + "). "
               "Завершуючись, потік переходить у список завершених, а пул дочікується "
               "його (" + c("WaitFor") + ") і звільняє — у потоці-моніторі та обов'язково "
               "в " + c("Destroy") + ". Тому після " + c("Pool.Free") + " жоден потік "
               "пулу вже не виконується, а пул за замовчуванням не конфліктує з "
               "завершенням програми."))
    s.extend(example("pool_lifetime.pp",
                     "Threads before: 1\nThreads while pool is alive: 6\n"
                     "Threads after Free: 1"))
    s.append(p("<b>Зверніть увагу.</b> Обробник " + c("OnThreadTerminate") + " виконується "
               "в потоці, що завершується. " + c("Destroy") + " чекає й на нього, тож "
               "повільний обробник уповільнює " + c("Free") + ". Не викликайте з нього "
               + c("TThread.Synchronize") + ", якщо пул знищується з головного потоку.",
               S_NOTE))

    s.append(PageBreak())
    s.append(p("4. Перевірка гонок даних: модуль fpvalgrind", S_H1))
    s.append(p("helgrind і drd не бачать синхронізації без " + c("pthread_mutex") + ": "
               "спін-блокувань, атомарних операцій, кешу об'єктів. Модуль "
               + c("fpvalgrind") + " (пакет fcl-base) передає valgrind підказки. Код "
               "анотацій є лише в збірці з " + c("-dFPC_VALGRIND") + "; у звичайній збірці "
               "його немає. Запити реалізовано для x86_64 (System V); на інших "
               "платформах це порожні процедури."))
    s.append(table([
        ["Процедура", "Призначення"],
        [c("VgHappensBefore(p)"), "Потік «публікує» все, що записав, з міткою p "
         "(перед звільненням блокування / записом прапорця)"],
        [c("VgHappensAfter(p)"), "Потік бачить усе, що опубліковано з міткою p (після "
         "захоплення блокування / читання прапорця)"],
        [c("VgCleanMemory(p, n)"), "Пам'ять «нова»: для об'єктів, перевикористаних "
         "поза менеджером пам'яті (кеші)"],
        [c("VgBenignRace(p, n)"), "Не перевіряти поле, яке свідомо читається без "
         "блокування (прапорці)"],
        [c("VgRunningOnValgrind"), c("True") + ", якщо програма працює під valgrind"],
    ], [48, 122]))
    s.append(Spacer(1, 6))
    s.extend(example("valgrind_annotate.pp", "Running on valgrind: TRUE\nPayload: 42"))
    s.append(p("Результат helgrind для цього прикладу:", S_H2))
    s.append(table([
        ["Збірка", "Звіт helgrind"],
        ["без анотацій", "хибна гонка на " + c("Payload") + " (рядок 42 ↔ рядок 22) + "
         "1 відома особливість RTL у " + c("TThread.WaitFor")],
        [c("-dFPC_VALGRIND"), "лише відома особливість RTL у " + c("TThread.WaitFor")],
    ], [40, 130]))
    s.append(Spacer(1, 6))
    s.append(p("Анотації вже додано в " + c("TSpinLock") + " (syncobjs) і в "
               + c("System.Threading") + ". Для своїх програм: позначайте анотаціями "
               "лише ту синхронізацію, яку helgrind не бачить сам."))

    s.append(p("5. Тести: ThreadCheck замість Assert у потоках", S_H1))
    s.append(p(c("Assert*") + " у робочому потоці кидає виняток у цьому потоці, а не в "
               "тесті, тож невдача може загубитися; лічильники fpcunit не потокобезпечні. "
               "У " + c("TThreadedTestCase") + " (тести System.Threading) є "
               + c("ThreadCheck") + " і " + c("ThreadCheckSame") + ": невдача "
               "записується, а " + c("TearDown") + " після завершення всіх потоків валить "
               "тест з переліком невдач."))
    s.append(code_block(
        "procedure TMyTest.DoWork(Sender: TObject);   // виконується в потоці пулу\n"
        "begin\n"
        "  // AssertSame('Sender', Self, Sender);     // НІ: виняток у чужому потоці\n"
        "  ThreadCheckSame('Sender', Self, Sender);   // ТАК: невдачу покаже TearDown\n"
        "  ThreadCheck(Counter > 0, 'Counter must be positive');\n"
        "end;", title="Фрагмент тесту"))
    s.append(Spacer(1, 3))
    s.append(code_block(
        "TestExecuteWork  Failed: [TEARDOWN] Checks failed in worker threads:\n"
        "[thread 127149134571200] Counter must be positive",
        style=S_OUT, title="Так виглядає невдача:"))
    s.append(p("Тестові потоки (" + c("TNotifyThread") + ", " + c("TTaskThread") + ") "
               "тепер не " + c("FreeOnTerminate") + ": " + c("TearDown") + " дочікується "
               "їх і звільняє.", S_NOTE))

    s.append(KeepTogether([
        p("6. Інструменти (tests/concurrency/)", S_H1),
        table([
            ["Скрипт", "Що робить"],
            [c("fpcenv.sh"), "Налаштування для компіляції свіжим компілятором з дерева "
             "джерел (окремий файл налаштувань на кожен запуск)"],
            [c("build-testcompat.sh [тека]"), "Збирає тести vcl-compat з поточних "
             "джерел"],
            [c("stress.sh [N] [таймаут]"), "Запускає кожен набір тестів System.Threading "
             "N разів окремим процесом; виявляє рідкісні падіння й зависання"],
            [c("build-testcompat-valgrind.sh"), "Збірка з " + c("-dFPC_VALGRIND") + " і "
             "обходом падіння valgrind на символах налагодження FPC"],
            [c("fpc-helgrind.supp"), "Винятки для навмисної поведінки RTL"],
            [c("hgsummary.py лог"), "Групує звіт helgrind за місцями конфліктів"],
        ], [58, 112]),
    ]))
    s.append(Spacer(1, 6))
    s.append(code_block(
        "tests/concurrency/build-testcompat.sh\n"
        "tests/concurrency/out/testcompat --all --format=plain\n"
        "tests/concurrency/stress.sh 50 60\n"
        "\n"
        "tests/concurrency/build-testcompat-valgrind.sh\n"
        "valgrind --tool=helgrind --suppressions=tests/concurrency/fpc-helgrind.supp \\\n"
        "  --log-file=hg.log tests/concurrency/out-valgrind/testcompat --suite=TTestTask\n"
        "tests/concurrency/hgsummary.py hg.log", title="Типовий цикл перевірки"))

    s.append(p("7. Зміни в RTL (Linux / Unix)", S_H1))
    s.append(table([
        ["Файл", "Зміна"],
        [c("rtl/unix/tthread.inc"), "Новий потік більше не перезаписує без "
         "синхронізації службовий прапорець " + c("FSuspendedInternal") + " (його вже "
         "встановлено до старту потоку)"],
        [c("rtl/unix/cthreads.pp"), "Список ідентифікаторів потоків " + c("ThreadTidList")
         + " читається лише під " + c("ThreadTidLock")],
    ], [45, 125]))
    s.append(Spacer(1, 5))
    s.append(p("Ці зміни стосуються кожної програми на FPC під Linux. Повний набір тестів "
               "компілятора (10 442 тести) дає той самий список падінь, що й до змін.",
               S_NOTE))

    s.append(p("8. Перевірено", S_H1))
    s.append(table([
        ["Перевірка", "Результат"],
        ["Тести vcl-compat", "702 тести; 2 відомі падіння, не пов'язані з потоками"],
        ["Стрес-прогін", "9 наборів × 50 повторів: 450/450; після останніх змін у "
         "тестах — 20 повторів: 180/180. Без падінь і зависань"],
        ["Тести компілятора", "той самий список падінь, що й до змін"],
        ["helgrind", "0 попереджень у System.Threading; решта — відомі категорії RTL"],
    ], [45, 125]))

    s.append(p("9. Відомі обмеження", S_H1))
    s.append(p("• helgrind не розуміє атомарного лічильника посилань " +
               c("TInterfacedObject") + " (модуль System) і показує хибні конфлікти під "
               "час звільнення інтерфейсних об'єктів. На роботу програм не впливає."))
    s.append(p("• " + c("TThread.WaitFor") + " опитує прапорець завершення без блокування; "
               "після нього " + c("pthread_join") + " дає повну синхронізацію. "
               "Доброякісно."))
    s.append(p("• Анотації valgrind реалізовано лише для x86_64."))
    s.append(p("Докладніше: " + c("docs/concurrency/valgrind.md") + ", "
               + c("docs/concurrency/baseline.md") + ", " + c("CHANGELOG-concurrency.md")
               + ".", S_NOTE))

    doc = SimpleDocTemplate(OUT, pagesize=A4, leftMargin=20 * mm, rightMargin=20 * mm,
                            topMargin=18 * mm, bottomMargin=20 * mm,
                            title="Багатопоточність у Free Pascal: що нового",
                            author="FPC feature/concurrency")
    doc.build(flat(s), onFirstPage=footer, onLaterPages=footer)
    print(OUT)


if __name__ == "__main__":
    build()
