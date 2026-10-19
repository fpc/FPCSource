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
from reportlab.platypus import (CondPageBreak, KeepTogether, PageBreak, Paragraph,
                                SimpleDocTemplate, Spacer, Table, TableStyle,
                                XPreformatted)

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
S_CAPTION_LONG = ParagraphStyle("captionlong", parent=S_CAPTION, keepWithNext=False)


def c(text):
    """Inline code inside a paragraph."""
    return f'<font name="Mono" size="8.5">{escape(text)}</font>'


def p(text, style=S_BODY):
    return Paragraph(text, style)


def CodeBlock(text, style):
    """Code listing: keeps line breaks, draws background, splits across pages."""
    return XPreformatted(escape(text), style)


def code_block(text, style=S_CODE, title=None):
    """Caption (kept with the code) + a code block that may split across pages."""
    pre = CodeBlock(text.rstrip("\n"), style)
    if not title:
        return pre
    if text.count("\n") < 12:
        # Short block: caption and code on the same page.
        return KeepTogether([Paragraph(escape(title), S_CAPTION), pre])
    # Long block: it is split across pages; just avoid an orphan caption.
    return _Seq([CondPageBreak(40 * mm), Paragraph(escape(title), S_CAPTION_LONG), pre])


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


def listing(name):
    with open(os.path.join(EXAMPLES, name), encoding="utf-8") as f:
        return f.read()


def comparison():
    """Section 10: classic TThread vs System.Threading, measured on 12 cores."""
    s = [PageBreak()]
    s.append(p("10. Порівняння: класичний TThread і System.Threading", S_H1))
    s.append(p("Дві невеликі програми, кожна у двох варіантах: лише " + c("TThread") +
               " з RTL і " + c("System.Threading") + ". Обидва варіанти дають однаковий "
               "результат; заміри на 12 ядрах, компіляція " + c("-O2") + ", по 3 запуски. "
               "Повні тексти — у додатку."))

    s.append(p("Приклад А. Обробка даних: прості числа до 5 000 000", S_H2))
    s.append(p("Порахувати кількість і суму простих чисел. Обчислювальна задача, "
               "одна операція над великим діапазоном."))
    s.append(table([
        ["", "А1: " + c("TThread"), "А2: " + c("System.Threading")],
        ["Результат", "348 513 простих, сума 838 596 693 108", "той самий"],
        ["Час", "201 / 201 / 302 мс", "<b>125 / 122 / 123 мс</b>"],
        ["Додатково", "—", "паралельно знаходить найбільше просте (4 999 999)"],
        ["Рядків коду", "80 (1 клас потоку)", "<b>52</b> (жодного класу)"],
        ["Ручна робота", "поділ діапазону, клас потоку, WaitFor, Free, збирання "
         "результатів, перевірка FatalException", "лише тіло ітерації"],
    ], [30, 70, 70]))
    s.append(Spacer(1, 5))
    s.append(p("<b>Чому А2 швидший.</b> Перевірка великих чисел триває довше, тому в А1 "
               "потоки з першими шматками рано простоюють, а останній працює найдовше "
               "(звідси й розкид 201–302 мс). " + c("TParallel.For") + " роздає роботу "
               "невеликими порціями, і вільний потік бере наступну — навантаження "
               "вирівнюється само. Щоб зробити так у А1, потрібна власна черга задач."))
    s.append(code_block(
        "TParallel.&For(1, N,\n"
        "  procedure(I: Integer)\n"
        "  begin\n"
        "    if IsPrime(I) then\n"
        "    begin\n"
        "      AtomicIncrement(Count);\n"
        "      InterlockedExchangeAdd64(Sum, I);\n"
        "    end;\n"
        "  end);", title="А2: уся паралельна частина"))

    s.append(p("Приклад Б. Оркестрація задач: звіт з 4 джерел", S_H2))
    s.append(p("Чотири незалежні джерела різної тривалості (150–400 мс), одне з них "
               "завершується помилкою. Вимоги: виконати одночасно, загальний ліміт 2 с, "
               "показати помилки, зібрати решту результатів. Так виглядає типова "
               "програма, що одночасно звертається до бази, файлів чи мережі."))
    s.append(table([
        ["", "Б1: " + c("TThread"), "Б2: " + c("System.Threading")],
        ["Результат", "помилка показана, 3 значення зібрано", "той самий"],
        ["Час", "400 / 401 / 401 мс", "402 / 401 / 401 мс"],
        ["Рядків коду", "88 (<b>5 класів</b>)", "<b>42</b> (жодного класу)"],
        ["Таймаут", "власна подія + лічильник (TThread.WaitFor без таймауту)",
         c("WaitForAll(..., 2000)")],
        ["Помилки", "перевірка " + c("FatalException") + " кожного потоку окремо",
         "усі в одному " + c("EAggregateException")],
        ["Типи результатів", "поле в кожному класі + приведення типу",
         c("IFuture<Int64>") + ", " + c("IFuture<string>") + ", ..."],
    ], [30, 70, 70]))
    s.append(Spacer(1, 5))
    s.append(p("Швидкість однакова: час визначає найдовше джерело (послідовно було б "
               "~1100 мс). Виграш — у коді: удвічі менше рядків, жодного допоміжного "
               "класу, помилки й таймаут — вбудовані."))
    s.append(code_block(
        "try\n"
        "  if not TTask.WaitForAll([Orders, TopCustomer, Average, Rate], 2000) then\n"
        "    WriteLn('Timeout: report is incomplete');\n"
        "except\n"
        "  on Agg: EAggregateException do      // усі помилки в одному місці\n"
        "    for E in Agg do\n"
        "      WriteLn('Error: ', E.Message);\n"
        "end;", title="Б2: очікування, таймаут і помилки"))
    s.append(p("<b>Знахідка.</b> Саме цей приклад виявив ще одну ваду trunk: цикл "
               + c("for E in Agg") + " видавав зайвий елемент за межами масиву, і "
               "програма падала з " + c("Access violation") + ". Виправлено (" +
               c("EAggregateException.GetEnumerator") + "), додано тести "
               + c("TestEnumerate") + " і " + c("TestEnumerateAfterAdd") + ".", S_NOTE))

    s.append(p("Приклад В. Повторювані порції задач: 20 раундів × 500 задач", S_H2))
    s.append(p("Типовий серверний сценарій: задачі надходять пачками по 500 штук, "
               "кожна триває ~0,1 мс. П'ять підходів на тій самій роботі (12 ядер):"))
    s.append(table([
        ["Підхід", "Час", "Прискорення", "Примітка"],
        ["1. Послідовно", "926 мс", "×1,00", "базова лінія"],
        ["2. Потік на кожну задачу", "898 мс", "×1,03",
         "10 000 разів створити/знищити потік ОС з'їдає весь виграш"],
        ["3. Робочі потоки + " + c("WaitFor"), "2031 мс", "×0,46",
         "у головному потоці " + c("WaitFor") + " опитує раз на 100 мс (20 × 100 мс)"],
        ["4. Робочі потоки + власна подія", "190 мс", "×4,87",
         "ручна черга + " + c("TSimpleEvent") + ", але потоки створюються на кожен раунд"],
        ["5. " + c("TParallel.For"), "<b>160 мс</b>", "<b>×5,79</b>",
         "пул перевикористовує потоки між раундами; 6 рядків коду"],
    ], [52, 22, 26, 70]))
    s.append(Spacer(1, 5))
    s.append(p("<b>Знахідка.</b> Під час перевірки цього прикладу виявилося, що "
               + c("TWorkStealingQueue.TrySteal(Item, 0)") + " на порожній черзі зайво "
               "виконував два системні виклики події (" + c("ResetEvent") + " + "
               + c("WaitFor(0)") + ") під спін-блокуванням списку черг пулу. Вільні "
               "потоки переходили в " + c("Sleep(1)") + " і приєднувалися до короткого "
               "циклу по одному. Після виправлення (" + c("TestStealNoWaitIsCheap") + ") "
               "час " + c("TParallel.For") + " зменшився до 160 мс.", S_NOTE))

    s.append(p("Приклад Г. Комплексний конвеєр розробника: стандартний TThread vs System.Threading (showcase_pipeline.pp)", S_H2))
    s.append(p("Одна програма виконує чотири типові прикладні задачі двічі — стандартним "
               "методом (класи-спадкоємці " + c("TThread") + " з ручною синхронізацією) і "
               "через " + c("System.Threading") + " — та звіряє збіг результатів, час і "
               "обсяг коду (12 ядер, " + c("-O2") + "):"))
    s.append(table([
        ["Задача", "Стандартний " + c("TThread"), c("System.Threading"), "Виграш"],
        ["1. Пошук першої помилки за індексом (" + c("Break") + ")",
         "101 мс · 38 рядків · 1 клас + ручний CAS-цикл для " + c("LowestIdx"),
         "<b>16 мс · 10 рядків</b> · " + c("State.Break") + " + " + c("LowestBreakIteration"),
         "<b>×6,3 швидше</b>, у 3,8 раза менше коду"],
        ["2. Пошук будь-якого збігу (" + c("Stop") + ")",
         "101 мс · 30 рядків · 1 клас + спільний прапорець " + c("StopFlag"),
         "<b>6 мс · 11 рядків</b> · " + c("State.Stop"),
         "<b>×16,8 швидше</b>, у 2,7 раза менше коду"],
        ["3. Пакетна валідація з винятками",
         "100 мс · 36 рядків · 1 клас + ручний обхід " + c("FatalException") + " до " + c("Free"),
         "<b>1 мс · 14 рядків</b> · " + c("try..except on EAggregateException"),
         "<b>×100 швидше</b>, помилки неможливо загубити"],
        ["4. Найшвидша репліка + паралельне зведення",
         "141 мс · 64 рядки · 2 класи + " + c("TSimpleEvent") + " + " + c("WinnerIdx"),
         "<b>41 мс · 16 рядків</b> · " + c("WaitForAny") + " + " + c("TParallel.Join"),
         "<b>×3,4 швидше</b>, у 4 рази менше коду"],
        ["<b>Разом по 4 задачах</b>",
         "<b>443 мс · 168 рядків · 5 класів</b>",
         "<b>64 мс · 51 рядок · 0 класів</b>",
         "<b>×6,9 швидше, −70% коду</b>"],
    ], [38, 46, 46, 40]))
    s.append(Spacer(1, 5))
    s.append(p("<b>Чому стандартний метод програє в 3,4–100 разів за часом на коротких "
               "етапах:</b> (1) створення й знищення потоків ОС на кожну задачу; "
               "(2) " + c("TThread.WaitFor") + " у головному потоці опитує завершення з "
               "кроком 100 мс (" + c("CheckSynchronize(100)") + "), тому кожне приєднання "
               "додає ~100 мс затримки. Пул " + c("System.Threading") + " тримає робочі "
               "потоки напоготові й синхронізується через легковагові події без затримки."))
    s.append(p("<b>Головні переваги нових інструментів при розробці програм:</b>"))
    s.append(p("• <b>Декларативність замість інфраструктурного коду.</b> Розробник описує "
               "<i>що</i> потрібно зробити (тіло ітерації, функцію майбутнього результату, "
               "перелік процедур у " + c("Join") + "), а не <i>як</i> ділити діапазони, "
               "створювати класи-спадкоємці " + c("TThread") + ", крутити CAS-цикли, "
               "сигналізувати події чи звільняти пам'ять."))
    s.append(p("• <b>Детермінована семантика зупинки (" + c("Break") + " vs " + c("Stop") +
               ").</b> " + c("Break") + " зберігає послідовний інваріант «знайти перший за "
               "порядком індексу», тоді як " + c("Stop") + " дає максимальну економію часу, "
               "коли підходить будь-який результат. Зі звичайним " + c("TThread") + " кожен "
               "із цих режимів потребує окремого протоколу синхронізації."))
    s.append(p("• <b>Наскрізна безпека щодо винятків.</b> У класичних потоках необроблений "
               "виняток тихо осідає в " + c("FatalException") + " (якщо програміст забув "
               "його перевірити до " + c("Free") + " — помилка зникає). У " +
               c("System.Threading") + " винятки з задач, майбутніх обчислень і паралельних "
               "циклів гарантовано доставляються в потік-викликач як " +
               c("EAggregateException") + "."))
    s.append(p("• <b>Композиційність і перевикористання пулу.</b> Послідовні та вкладені "
               "виклики " + c("TParallel.For") + ", " + c("TParallel.Join") + " та " +
               c("TTask.Future") + " працюють на спільному пулі з крадіжкою роботи "
               "(work-stealing), автоматично балансуючи нерівномірне навантаження."))

    s.append(p("Підсумок порівняння", S_H2))
    s.append(table([
        ["Аспект", "Класичний " + c("TThread"), c("System.Threading")],
        ["Обсяг коду", "80–168 рядків", "42–52 рядки (−50…−70%)"],
        ["Допоміжні класи", "1–5 на задачу", "0"],
        ["Розподіл роботи", "вручну, статично", "автоматично, з вирівнюванням"],
        ["Ранній вихід із циклу", "власний CAS-цикл мінімуму і прапорці",
         c("State.Break") + " (за індексом) / " + c("State.Stop") + " (миттєво)"],
        ["Помилки в потоках", "легко пропустити (FatalException)", "повторно виникають "
         "у коді, що чекає (" + c("EAggregateException") + ")"],
        ["Таймаут і хеджування", "власні події й лічильники",
         c("WaitForAll") + " / " + c("WaitForAny") + " з таймаутом"],
        ["Потоки", "створюються щоразу (+100 мс у WaitFor)", "перевикористовуються пулом"],
    ], [38, 66, 66]))
    s.append(Spacer(1, 6))
    s.append(p("<b>Коли класичний TThread кращий:</b> довгоживучий окремий потік "
               "(фоновий сервіс, цикл повідомлень, потік на з'єднання з блокуючим I/O); "
               "потрібен контроль пріоритету чи прив'язки до ядра; дуже дрібні ітерації, "
               "де накладні витрати на кожну ітерацію й атомарні операції з'їдають "
               "виграш (тоді обробляйте дані блоками з локальним підсумком)."))
    s.append(p("<b>Чому це стало практичним саме тут.</b> API " + c("System.Threading") +
               " був у trunk і раніше, але без виправлень гілки варіант А2 міг випадково "
               "виконатися в одному потоці (у 12 разів повільніше) або загубити чи "
               "подвоїти ітерації з неправильним результатом, Б2 — упасти під час "
               "перебору помилок, а приклад Г — впасти з AV на " + c("TLoopState") +
               ", загубити виняток циклу та назавжди зависнути на " + c("TParallel.Join") + "."))
    s.append(p("Приклад Д. Примітиви SyncObjs Фази 2 vs ручна синхронізація (syncobjs_phase2.pp)", S_H2))
    s.append(p("Комплексний тест телеметрії (4000 повідомлень у багатопотоковому конвеєрі, "
               "100 000 читань таблиці калібрування з фоновими оновленнями, динамічна група "
               "воркерів) та 40-фазна 1D-симуляція теплопровідності на сітці з 512 комірок:"))
    s.append(table([
        ["Підхід", "Час", "Інфраструктурний код", "Примітиви"],
        ["А. Класичний ручний", "45 мс", "~165 рядків (2 власні класи черги й бар'єра)",
         c("TCriticalSection") + " + " + c("TEvent") + " + ручні лічильники"],
        ["Б. SyncObjs (Фаза 2)", "<b>26 мс</b>", "<b>4 декларації класів (0 ручних черг)</b>",
         c("TSlimReaderWriterLock") + " + " + c("TChannel<T>") + " + " +
         c("TCountdownEvent") + " + " + c("TBarrier")],
    ], [42, 22, 54, 52]))
    s.append(Spacer(1, 5))
    s.append(p("<b>Чому Фаза 2 у ~1,7 раза швидша й не потребує ~165 рядків коду:</b> "
               "(1) " + c("TSlimReaderWriterLock") + " дозволяє всім потокам-читачам читати "
               "таблицю калібрування паралельно за одну атомарну CAS-інструкцію у просторі "
               "користувача (тоді як " + c("TCriticalSection") + " серіалізує читачів); "
               "(2) " + c("TChannel<T>") + " сигналізує подію ядра лише за наявності потоків, "
               "що реально очікують (" + c("FWaitingReceivers > 0") + " / " +
               c("FWaitingSenders > 0") + "), та підтримує ідіоматичний перебір " +
               c("for Msg in TelemetryChannel do") + " без алокацій у купі; "
               "(3) " + c("TBarrier") + " автоматично виконує пост-фазну агрегацію рівно "
               "одним потоком з подвійним буфером подій."))
    s.append(p("Приклад Е. Неблокувальні продовження (.ContinueWith / .ThenBy) та TCancellationToken (continuations_cancel.pp)", S_H2))
    s.append(p("Конвеєрна 4-етапна обробка 64 пакетів по 2000 елементів, кооперативне скасування "
               "дерева задач із вкладеними циклами " + c("TParallel.For") + " та автоматичне "
               "прокидання винятків крізь конвеєр:"))
    s.append(table([
        ["Підхід", "Час", "Блокування потоків пулу", "Скасування та помилки"],
        ["А. Стандартний (" + c(".Value") + " / " + c(".Wait") + ")", "80 мс",
         "4 блокування на кожен пакет (ризик виснаження пулу)",
         "Ручні прапорці у кожній задачі + ручний " + c("try..except")],
        ["Б. Неблокувальний (" + c(".ThenBy") + " + " + c("TCancellationToken") + ")", "<b>25 мс</b>",
         "<b>0 блокувань (асинхронний граф DAG)</b>",
         "Єдиний " + c("TCancellationTokenSource") + " + авто-прокидання помилок"],
    ], [42, 20, 54, 54]))
    return s


def appendix():
    s = [PageBreak(), p("Додаток. Повні тексти прикладів порівняння", S_H1)]
    outputs = {
        "primes_classic.pp": "Primes: 348513, sum: 838596693108\n"
                             "Threads: 12, time: 201 ms",
        "primes_threading.pp": "Primes: 348513, sum: 838596693108\n"
                               "Largest prime <= N: 4999999\nTime: 122 ms",
        "report_classic.pp": "Error: Rates service: connection refused\nOrders: 1234\n"
                             "Top customer: Acme Ltd\nAverage check: 87.5\nTime: 401 ms",
        "report_threading.pp": "Error: Rates service: connection refused\nOrders: 1234\n"
                               "Top customer: Acme Ltd\nAverage check: 87.5\nTime: 401 ms",
        "batch_processing.pp": "20 rounds x 500 jobs, 12 logical CPUs\n"
                               "1. Sequential                               926 ms  x 1.00  OK\n"
                               "2. TThread per job                          898 ms  x 1.03  OK\n"
                               "3. Worker threads + WaitFor                2031 ms  x 0.46  OK\n"
                               "4. Worker threads + event (expert)          190 ms  x 4.87  OK\n"
                               "5. TParallel.For                            160 ms  x 5.79  OK",
        "showcase_pipeline.pp": "=== Standard TThread vs System.Threading (12 logical CPUs) ===\n"
                                "1. Ordered Break (index=24000=24000):   TThread  101 ms (38 lines) | System.Threading  16 ms (10 lines)\n"
                                "2. Immediate Stop (completed=F): TThread  101 ms (30 lines) | System.Threading   6 ms (11 lines)\n"
                                "3. Exception handling (match=TRUE):  TThread  100 ms (36 lines) | System.Threading   1 ms (14 lines)\n"
                                "4. WaitForAny + Join (eu-central, 1249975000): TThread  141 ms (64 lines) | System.Threading  41 ms (16 lines)\n"
                                "Total helper thread classes:        TThread = 5 classes (168 lines) | System.Threading = 0 classes (51 lines)",
        "syncobjs_phase2.pp": "===========================================================================\n"
                              " ПОРІВНЯННЯ: Класична ручна синхронізація vs Нові примітиви SyncObjs (Фаза 2)\n"
                              "   Пакетів телеметрії: 4000 (читань таблиці калібрування: 100000)\n"
                              "   1D-симуляція теплопровідності: сітка 512 комірок x 40 фаз\n"
                              "===========================================================================\n"
                              " [A] Класичний метод (TCriticalSection + TEvent + ручна черга/бар'єр):   45 мс | дельта=0.3069\n"
                              " [B] SyncObjs Фаза 2 (TSlimRWLock + TChannel<T> + TCountdown + TBarrier):   26 мс | дельта=0.3069\n"
                              "---------------------------------------------------------------------------\n"
                              " Збіг результатів чисельної симуляції: ТАК (OK)\n"
                              " Скорочення інфраструктурного коду синхронізації: зі ~165 рядків до 4 декларацій класів\n"
                              "===========================================================================",
        "continuations_cancel.pp": "==============================================================================\n"
                                   " Демонстрація Фази 1.1: Неблокувальні продовження (.ContinueWith / .ThenBy)\n"
                                   " та кооперативне скасування (TCancellationToken / TCancellationTokenSource)\n"
                                   "==============================================================================\n"
                                   "  [Підхід A] Класичний блокувальний (.Value/.Wait) :   80 мс (оброблено 64 пакетів)\n"
                                   "  [Підхід B] Неблокувальний (.ThenBy/.ContinueWith):   25 мс (оброблено 64 пакетів)\n"
                                   "=== ЧАСТИНА 2: Кооперативне скасування дерева операцій (TCancellationToken) ===\n"
                                   "  [Token Callback] Отримано сигнал скасування! Починаємо згортання дерева задач...\n"
                                   "  Статус кореневої задачі       : 5 (очікувано Canceled = 5)\n"
                                   "  Гілка OnlyOnCompleted         : статус=5, викликів=0\n"
                                   "  Гілка OnlyOnCanceled          : статус=3, викликів=1\n"
                                   "=== ЧАСТИНА 3: Автоматичне прокидання винятків крізь конвеєр .ThenBy ===\n"
                                   "  Статус Stage1 / Stage2 / Stage3 : 6 / 6 / 6 (Exception = 6)\n"
                                   "  Результат компенсації           : Перехоплено у OnlyOnFaulted (Stage3.Status = Exception)",
    }
    for name, out in outputs.items():
        s.extend(example(name, out))
    return s


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
        [c("ContinueWith") + " / " + c("ThenBy") + " / " + c("TCancellationToken"),
         "Отримання результату " + c("IFuture<T>") + " блокувало потік у " + c(".Wait") +
         "/" + c(".Value") + "; скасування " + c("ITask.Cancel") + " діяло лише на окрему задачу",
         "Неблокувальні конвеєри " + c(".ContinueWith") + ", " + c(".ThenBy<T,TResult>") +
         ", " + c(".Map") + " та кооперативний " + c("TCancellationToken") + " / " +
         c("TCancellationTokenSource") + " для дерев задач і " + c("TParallel.For")],
        [c("TParallel.For"), "Іноді виконувався в одному потоці (20,7 с замість 1,7 с); "
         "ітерації могли губитися або виконуватися двічі; цикли з " + c("TLoopState") +
         " падали з AV; " + c("Break") + "/" + c("Stop") + " не зупиняли цикл; "
         "винятки в ітераціях губилися",
         "Стабільно паралельний; кожна ітерація рівно один раз; працюють " +
         c("TLoopState") + ", " + c("Break") + ", " + c("Stop") + " і передавання винятків"],
        [c("TParallel.Join"), "Не запускав задачу; " + c("Params") + " зі сміттям зі "
         "стеку; " + c("Parent := Self") + " вічно блокував " + c("Wait"),
         "Запускає процедури паралельно й коректно завершує " + c("Wait")],
        [c("TTask.Wait") + ", " + c("WaitForAny"), "Читання стану без бар'єра / без "
         "блокування", "Коректно і на x86, і на ARM64"],
        [c("TThreadPool.Free"), "Повертав керування, поки потоки пулу ще працювали; "
         "запис у звільнену пам'ять монітором", "Дочікується фактичного завершення "
         "всіх потоків"],
        ["valgrind / helgrind", "Сотні хибних попереджень; падіння valgrind", "Модуль "
         + c("fpvalgrind") + ", анотації, скрипти; 0 попереджень у System.Threading"],
        [c("for E in EAggregateException"), "Видавав зайвий елемент за межами "
         "масиву — падіння (Access violation)", "Рівно ті помилки, що зібрано"],
        ["Тести", c("Assert") + " з робочих потоків міг загубити невдачу",
         c("ThreadCheck") + " / " + c("ThreadCheckSame")],
    ], [38, 62, 70]))

    s.append(p("1. Паралельний цикл TParallel.For", S_H1))
    s.append(p("Виправлено низку помилок. (1) На щойно створеному («холодному») пулі "
               "вільні потоки не помічали роботу в локальній черзі кореневої задачі й "
               "спали до 40 с, тож увесь цикл виконувався одним потоком. (2) Потік "
               "забирав діапазон ітерацій з одним розміром кроку, а виконував з іншим, "
               "тому частина ітерацій губилася або виконувалася двічі при "
               + c("TLoopResult.Completed = True") + ". (3) " + c("TLoopState") +
               " не створювався (" + c("nil") + "), " + c("Break") + " і " + c("Stop") +
               " не зупиняли цикл, а винятки з дочірніх реплік губилися (" +
               c("EAggregateException.Count = 0") + ")."))
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
        ["Тести vcl-compat", "710 тестів; 2 відомі падіння, не пов'язані з потоками"],
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
    s.append(p("Докладніше: " + c("docs/concurrency/guide-system-threading.md") + ", "
               + c("docs/concurrency/valgrind.md") + ", " + c("docs/concurrency/adr/")
               + ", " + c("docs/concurrency/baseline.md") + ", fpdoc-описи "
               + c("packages/fcl-base/xml/fpvalgrind.xml") + " і "
               + c("packages/vcl-compat/xml/system.threading.xml") + ", "
               + c("CHANGELOG-concurrency.md") + ".", S_NOTE))

    s.extend(comparison())
    s.extend(appendix())

    doc = SimpleDocTemplate(OUT, pagesize=A4, leftMargin=20 * mm, rightMargin=20 * mm,
                            topMargin=18 * mm, bottomMargin=20 * mm,
                            title="Багатопоточність у Free Pascal: що нового",
                            author="FPC feature/concurrency")
    doc.build(flat(s), onFirstPage=footer, onLaterPages=footer)
    print(OUT)


if __name__ == "__main__":
    build()
