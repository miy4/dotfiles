// ========================== KeySnail Init File =========================== //

// この領域は, GUI により設定ファイルを生成した際にも引き継がれます
// 特殊キー, キーバインド定義, フック, ブラックリスト以外のコードは, この中に書くようにして下さい
// ========================================================================= //
//{{%PRESERVE%
// ここにコードを入力して下さい

// 2011.01.23 Prefer LDRize setting
plugins.options["prefer_ldrize.keymap"] = {
     ":"   : null,
     "j"   : null,
     "k"   : null,
     "p"   : null,
     "v"   : null,
     "o"   : null,
     "J"   : function () { goDoCommand("cmd_scrollLineDown"); },
     "K"   : function () { goDoCommand("cmd_scrollLineUp"); },
};

// 2010.07.11 site local keymap setting
var local = {};
plugins.options["site_local_keymap.local_keymap"] = local;

function fake(k, i) function () { key.feed(k, i); };
function pass(k, i) [k, fake(k, i)];
function ignore(k, i) [k, null];

local["^https?://mail.google.com/mail/"] = [
    pass(['g', 'i'], 3),
    pass(['g', 's'], 3),
    pass(['g', 't'], 3),
    pass(['g', 'd'], 3),
    pass(['g', 'a'], 3),
    pass(['g', 'c'], 3),
    pass(['g', 'k'], 3),
    // thread list
    pass(['*', 'a'], 3),
    pass(['*', 'n'], 3),
    pass(['*', 'r'], 3),
    pass(['*', 'u'], 3),
    pass(['*', 's'], 3),
    pass(['*', 't'], 3),
    // navigation
    ['u', null],
    ['k', null],
    ['j', null],
    ['o', null],
    ['p', null],
    ['n', null],
    // application
    ['c', null],
    ['/', null],
    ['q', null],
    ['?', null],
    // manipulation
    ['x', null],
    ['s', null],
    ['y', null],
    ['e', null],
    ['m', null],
    ['!', null],
    ['#', null],
    ['r', null],
    ['R', null],
    ['a', null],
    ['A', null],
    ['f', null],
    ['F', null],
    ['N', null],
    pass(['<tab>', 'RET'], 3),
    ['ESC', null],
    [']', null],
    ['[', null],
    ['z', null],
    ['.', null],
    ['I', null],
    ['U', null],
    ['C-s', null],
    ['T', null]
];

local["^https?://www.google.(co.jp|com)/reader/view/"] = [
    // jump
    pass(["g", "h"]),
    pass(["g", "a"]),
    pass(["g", "s"]),
    pass(["g", "S"]),
    pass(["g", "u"]),
    pass(["g", "t"]),
    pass(["g", "T"]),
    pass(["g", "d"]),
    pass(["g", "f"]),
    pass(["g", "F"]),
    pass(["g", "c"]),
    pass(["g", "C"]),
    pass(["g", "e"]),
    pass(["g", "p"]),
    // navigation
    ["j", null],
    ["k", null],
    ["n", null],
    ["p", null],
    ["N", null],
    ["P", null],
    ["X", null],
    ["o", null],
    // item
    ["s", null],
    ["L", null],
    ["t", null],
    ["e", null],
    ["S", null],
    ["d", null],
    ["v", null],
    ["o", null],
    ["c", null],
    ["C", null],
    ["m", null],
    ["A", null],
    ["T", null],
    // application
    ["r", null],
    ["u", null],
    ["1", null],
    ["2", null],
    ["/", null],
    ["a", null],
    ["=", null],
    ["-", null]
];

local["^http://reader.livedoor.com/reader/"] = [
    ['r', null]
];

local["^https?://feedly.com/"] = [
    ['j', null],
    ['k', null]
];
// HoK setting
plugins.options["hok.unique_only"] = true;

// ime off
// http://keysnail.g.hatena.ne.jp/mooz/20110320/1300641715
// http://nishikawasasaki.hatenablog.com/entry/2013/05/15/214147
style.register(['#urlbar *|input {',
'ime-mode : inactive !important;',
'}'].join("\n"));

// 2012.01.24 Tanything setting
plugins.options["tanything_opt.keymap"] = {
    "C-z"   : "prompt-toggle-edit-mode",        //ローカルキーバインドモードを抜けて編集モードに移行
    "SPC"   : "prompt-next-page",               //一画面スクロールダウン
    "b"   : "prompt-previous-page",           //一画面スクロールアップ
    "j"     : "prompt-next-completion",         //次のアイテムに移動
    "k"     : "prompt-previous-completion",     //前のアイテム移動
    "g"     : "prompt-beginning-of-candidates", //最初のアイテムに移動
    "G"     : "prompt-end-of-candidates",       //最後のアイテムに移動
    "D"     : "prompt-cancel",                  //プロンプトを消す
    // Tanything specific actions
    "o"     : "localOpen",                      //選択したタブを開く
    "q"     : "localClose",                     //選択したタブを閉じる
    "l"     : "localLeftclose",                 //選択したタブの左にあるタブをすべて閉じる
    "r"     : "localRightclose",                //選択したタブの右にあるタブをすべて閉じる
    "a"   : "localAllclose",                  //選択したタブ以外をすべて閉じる(キーバインド”a”だと押し間違いがあるので"M-a"に変更。)
    "d"     : "localDomainclose",
    "c"     : "localClipUT",                    //選択したタブのタイトルとURLをHTMLタグつきでコピー
    "C"     : "localClipU",                     //選択したタブのURLをHTMLタグつきでコピー
    "e"     : "localMovetoend",                 //選択したタブを末尾に移動
    "p"     : "localTogglePin",                 
};

// 2012.01.25 LDRnail setting
plugins.options["ldrnail.exclude_urls"] = [
    "^https?://mail.google.com/mail/.*",
    "^https?://www.google.(co.jp|com)/reader/view/.*",
];
 
plugins.options["ldrnail.keybind"] = {
    'j': 'next',
    'k': 'prev',
    'p': 'pin',
    'l': 'list',
    'f': 'focus',
    'v': 'view',
    'o': 'open',
    's': 'siteinfo',
};

// 2012.01.25 Caret hint setting
plugins.options["caret_hint.head_key"] = "m"; // c -> m
plugins.options["caret_hint.tail_key"] = "M"; // C -> M


//}}%PRESERVE%
// ========================================================================= //

// ========================= Special key settings ========================== //

key.quitKey              = "C-g";
key.helpKey              = "<f1>";
key.escapeKey            = "C-q";
key.macroStartKey        = "<f3>";
key.macroEndKey          = "<f4>";
key.universalArgumentKey = "C-u";
key.negativeArgument1Key = "C--";
key.negativeArgument2Key = "C-M--";
key.negativeArgument3Key = "M--";
key.suspendKey           = "<f2>";

// ================================= Hooks ================================= //

hook.setHook('KeyBoardQuit', function (aEvent) {
    if (key.currentKeySequence.length) {
        return;
    }
    command.closeFindBar();
    var marked = command.marked(aEvent);
    if (util.isCaretEnabled()) {
        if (marked) {
            command.resetMark(aEvent);
        } else {
            if ("blur" in aEvent.target) {
                aEvent.target.blur();
            }
            gBrowser.focus();
            _content.focus();
        }
    } else {
        goDoCommand("cmd_selectNone");
    }
    if (KeySnail.windowType === "navigator:browser" && !marked) {
        key.generateKey(aEvent.originalTarget, KeyEvent.DOM_VK_ESCAPE, true);
    }
});

// ============================= Key bindings ============================== //

key.setGlobalKey('C-M-r', function (ev) {
    userscript.reload();
}, '設定ファイルを再読み込み', true);

key.setGlobalKey('M-x', function (ev, arg) {
    ext.select(arg, ev);
}, 'エクステ一覧表示', true);

key.setGlobalKey('M-:', function (ev) {
    command.interpreter();
}, 'JavaScript のコードを評価', true);

key.setGlobalKey(['<f1>', 'b'], function (ev) {
    key.listKeyBindings();
}, 'キーバインド一覧を表示');

key.setGlobalKey(['<f1>', 'F'], function (ev) {
    openHelpLink("firefox-help");
}, 'Firefox のヘルプを表示');

key.setGlobalKey('C-m', function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_RETURN, true);
}, 'リターンコードを生成');

key.setGlobalKey(['C-x', 'l'], function (ev) {
    command.focusToById("urlbar");
}, 'ロケーションバーへフォーカス', true);

key.setGlobalKey(['C-x', 'g'], function (ev) {
    command.focusToById("searchbar");
}, '検索バーへフォーカス', true);

key.setGlobalKey(['C-x', 't'], function (ev) {
    command.focusElement(command.elementsRetrieverTextarea, 0);
}, '最初のインプットエリアへフォーカス', true);

key.setGlobalKey(['C-x', 's'], function (ev) {
    command.focusElement(command.elementsRetrieverButton, 0);
}, '最初のボタンへフォーカス', true);

key.setGlobalKey(['C-x', 'k'], function (ev) {
    BrowserCloseTabOrWindow();
}, 'タブ / ウィンドウを閉じる');

key.setGlobalKey(['C-x', 'K'], function (ev) {
    closeWindow(true);
}, 'ウィンドウを閉じる');

key.setGlobalKey(['C-x', 'n'], function (ev) {
    OpenBrowserWindow();
}, 'ウィンドウを開く');

key.setGlobalKey(['C-x', 'C-c'], function (ev) {
    goQuitApplication();
}, 'Firefox を終了', true);

key.setGlobalKey(['C-x', 'o'], function (ev, arg) {
    command.focusOtherFrame(arg);
}, '次のフレームを選択');

key.setGlobalKey(['C-x', '1'], function (ev) {
    window.loadURI(ev.target.ownerDocument.location.href);
}, '現在のフレームだけを表示', true);

key.setGlobalKey(['C-x', 'C-f'], function (ev) {
    BrowserOpenFileWindow();
}, 'ファイルを開く', true);

key.setGlobalKey(['C-x', 'C-s'], function (ev) {
    saveDocument(window.content.document);
}, 'ファイルを保存', true);

key.setGlobalKey('M-w', function (ev) {
    command.copyRegion(ev);
}, '選択中のテキストをコピー', true);

key.setGlobalKey('C-s', function (ev) {
    command.iSearchForwardKs(ev);
}, 'Emacs ライクなインクリメンタル検索', true);

key.setGlobalKey('C-r', function (ev) {
    command.iSearchBackwardKs(ev);
}, 'Emacs ライクな逆方向インクリメンタル検索', true);

key.setGlobalKey(['C-c', 'u'], function (ev) {
    undoCloseTab();
}, '閉じたタブを元に戻す');

key.setGlobalKey(['C-c', 'C-c', 'C-v'], function (ev) {
    toJavaScriptConsole();
}, 'Javascript コンソールを表示', true);

key.setGlobalKey(['C-c', 'C-c', 'C-c'], function (ev) {
    command.clearConsole();
}, 'Javascript コンソールの表示をクリア', true);

key.setGlobalKey('C-M-l', function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(1, true);
}, 'ひとつ右のタブへ');

key.setGlobalKey('C-M-h', function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(-1, true);
}, 'ひとつ左のタブへ');

key.setViewKey([['C-n'], ['j']], function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_DOWN, true);
}, '一行スクロールダウン');

key.setViewKey([['C-p'], ['k']], function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_UP, true);
}, '一行スクロールアップ');

key.setViewKey([['C-f'], ['.']], function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_RIGHT, true);
}, '右へスクロール');

key.setViewKey([['C-b'], [',']], function (ev) {
    key.generateKey(ev.originalTarget, KeyEvent.DOM_VK_LEFT, true);
}, '左へスクロール');

key.setViewKey([['M-v'], ['b']], function (ev) {
    goDoCommand("cmd_scrollPageUp");
}, '一画面分スクロールアップ');

key.setViewKey('C-v', function (ev) {
    goDoCommand("cmd_scrollPageDown");
}, '一画面スクロールダウン');

key.setViewKey([['M-<'], ['g']], function (ev) {
    goDoCommand("cmd_scrollTop");
}, 'ページ先頭へ移動', true);

key.setViewKey([['M->'], ['G']], function (ev) {
    goDoCommand("cmd_scrollBottom");
}, 'ページ末尾へ移動', true);

key.setViewKey('C-.', function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(1, true);
}, 'ひとつ右のタブへ');

key.setViewKey('C-,', function (ev) {
    getBrowser().mTabContainer.advanceSelectedTab(-1, true);
}, 'ひとつ左のタブへ');

key.setViewKey(':', function (ev, arg) {
    shell.input(null, arg);
}, 'コマンドの実行', true);

key.setViewKey('R', function (ev) {
    BrowserReload();
}, '更新', true);

key.setViewKey('B', function (ev) {
    BrowserBack();
}, '戻る');

key.setViewKey('F', function (ev) {
    BrowserForward();
}, '進む');

key.setViewKey(['C-x', 'h'], function (ev) {
    goDoCommand("cmd_selectAll");
}, 'すべて選択', true);

key.setViewKey('f', function (ev) {
    command.focusElement(command.elementsRetrieverTextarea, 0);
}, '最初のインプットエリアへフォーカス', true);

key.setViewKey('M-p', function (ev) {
    command.walkInputElement(command.elementsRetrieverButton, true, true);
}, '次のボタンへフォーカスを当てる');

key.setViewKey('M-n', function (ev) {
    command.walkInputElement(command.elementsRetrieverButton, false, true);
}, '前のボタンへフォーカスを当てる');

key.setEditKey(['C-x', 'h'], function (ev) {
    command.selectAll(ev);
}, '全て選択', true);

key.setEditKey([['C-x', 'u'], ['C-_']], function (ev) {
    display.echoStatusBar("Undo!", 2000);
    goDoCommand("cmd_undo");
}, 'アンドゥ');

key.setEditKey(['C-x', 'r', 'd'], function (ev, arg) {
    command.replaceRectangle(ev.originalTarget, "", false, !arg);
}, '矩形削除', true);

key.setEditKey(['C-x', 'r', 't'], function (ev) {
    prompt.read("String rectangle: ", function (aStr, aInput) {command.replaceRectangle(aInput, aStr);}, ev.originalTarget);
}, '矩形置換', true);

key.setEditKey(['C-x', 'r', 'o'], function (ev) {
    command.openRectangle(ev.originalTarget);
}, '矩形行空け', true);

key.setEditKey(['C-x', 'r', 'k'], function (ev, arg) {
    command.kill.buffer = command.killRectangle(ev.originalTarget, !arg);
}, '矩形キル', true);

key.setEditKey(['C-x', 'r', 'y'], function (ev) {
    command.yankRectangle(ev.originalTarget, command.kill.buffer);
}, '矩形ヤンク', true);

key.setEditKey([['C-SPC'], ['C-@']], function (ev) {
    command.setMark(ev);
}, 'マークをセット', true);

key.setEditKey('C-o', function (ev) {
    command.openLine(ev);
}, '行を開く (Open line)');

key.setEditKey('C-\\', function (ev) {
    display.echoStatusBar("Redo!", 2000);
    goDoCommand("cmd_redo");
}, 'リドゥ');

key.setEditKey('C-a', function (ev) {
    command.beginLine(ev);
}, '行頭へ移動');

key.setEditKey('C-e', function (ev) {
    command.endLine(ev);
}, '行末へ');

key.setEditKey('C-f', function (ev) {
    command.nextChar(ev);
}, '一文字右へ移動');

key.setEditKey('C-b', function (ev) {
    command.previousChar(ev);
}, '一文字左へ移動');

key.setEditKey('M-f', function (ev) {
    command.forwardWord(ev);
}, '一単語右へ移動');

key.setEditKey('M-b', function (ev) {
    command.backwardWord(ev);
}, '一単語左へ移動');

key.setEditKey('C-n', function (ev) {
    command.nextLine(ev);
}, '一行下へ');

key.setEditKey('C-p', function (ev) {
    command.previousLine(ev);
}, '一行上へ');

key.setEditKey('C-v', function (ev) {
    command.pageDown(ev);
}, '一画面分下へ');

key.setEditKey('M-v', function (ev) {
    command.pageUp(ev);
}, '一画面分上へ');

key.setEditKey('M-<', function (ev) {
    command.moveTop(ev);
}, 'テキストエリア先頭へ');

key.setEditKey('M->', function (ev) {
    command.moveBottom(ev);
}, 'テキストエリア末尾へ');

key.setEditKey('C-d', function (ev) {
    goDoCommand("cmd_deleteCharForward");
}, '次の一文字削除');

key.setEditKey('C-h', function (ev) {
    goDoCommand("cmd_deleteCharBackward");
}, '前の一文字を削除');

key.setEditKey('M-d', function (ev) {
    command.deleteForwardWord(ev);
}, '次の一単語を削除');

key.setEditKey([['C-<backspace>'], ['M-<delete>']], function (ev) {
    command.deleteBackwardWord(ev);
}, '前の一単語を削除');

key.setEditKey('M-u', function (ev, arg) {
    command.wordCommand(ev, arg, command.upcaseForwardWord, command.upcaseBackwardWord);
}, '次の一単語を全て大文字に (Upper case)');

key.setEditKey('M-l', function (ev, arg) {
    command.wordCommand(ev, arg, command.downcaseForwardWord, command.downcaseBackwardWord);
}, '次の一単語を全て小文字に (Lower case)');

key.setEditKey('M-c', function (ev, arg) {
    command.wordCommand(ev, arg, command.capitalizeForwardWord, command.capitalizeBackwardWord);
}, '次の一単語をキャピタライズ');

key.setEditKey('C-k', function (ev) {
    command.killLine(ev);
}, 'カーソルから先を一行カット (Kill line)');

key.setEditKey('C-y', command.yank, '貼り付け (Yank)');

key.setEditKey('M-y', command.yankPop, '古いクリップボードの中身を順に貼り付け (Yank pop)', true);

key.setEditKey('C-M-y', function (ev) {
    if (!command.kill.ring.length) {
        return;
    }
    let ct = command.getClipboardText();
    if (!command.kill.ring.length || ct != command.kill.ring[0]) {
      command.pushKillRing(ct);
    }
    prompt.selector({message: "Paste:", collection: command.kill.ring, callback: function (i) {if (i >= 0) {key.insertText(command.kill.ring[i]);}}});
}, '以前にコピーしたテキスト一覧から選択して貼り付け', true);

key.setEditKey('C-w', function (ev) {
    goDoCommand("cmd_copy");
    goDoCommand("cmd_delete");
    command.resetMark(ev);
}, '選択中のテキストを切り取り (Kill region)', true);

key.setEditKey('M-n', function (ev) {
    command.walkInputElement(command.elementsRetrieverTextarea, true, true);
}, '次のテキストエリアへフォーカス');

key.setEditKey('M-p', function (ev) {
    command.walkInputElement(command.elementsRetrieverTextarea, false, true);
}, '前のテキストエリアへフォーカス');

key.setCaretKey([['C-a'], ['^']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectBeginLine") : goDoCommand("cmd_beginLine");
}, 'キャレットを行頭へ移動');

key.setCaretKey([['C-e'], ['$'], ['M->'], ['G']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectEndLine") : goDoCommand("cmd_endLine");
}, 'キャレットを行末へ移動');

key.setCaretKey([['C-n'], ['j']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectLineNext") : goDoCommand("cmd_scrollLineDown");
}, 'キャレットを一行下へ');

key.setCaretKey([['C-p'], ['k']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectLinePrevious") : goDoCommand("cmd_scrollLineUp");
}, 'キャレットを一行上へ');

key.setCaretKey([['C-f'], ['l']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectCharNext") : goDoCommand("cmd_scrollRight");
}, 'キャレットを一文字右へ移動');

key.setCaretKey([['C-b'], ['h'], ['C-h']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectCharPrevious") : goDoCommand("cmd_scrollLeft");
}, 'キャレットを一文字左へ移動');

key.setCaretKey([['M-f'], ['w']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectWordNext") : goDoCommand("cmd_wordNext");
}, 'キャレットを一単語右へ移動');

key.setCaretKey([['M-b'], ['W']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectWordPrevious") : goDoCommand("cmd_wordPrevious");
}, 'キャレットを一単語左へ移動');

key.setCaretKey([['C-v'], ['SPC']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectPageNext") : goDoCommand("cmd_movePageDown");
}, 'キャレットを一画面分下へ');

key.setCaretKey([['M-v'], ['b']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectPagePrevious") : goDoCommand("cmd_movePageUp");
}, 'キャレットを一画面分上へ');

key.setCaretKey([['M-<'], ['g']], function (ev) {
    ev.target.ksMarked ? goDoCommand("cmd_selectTop") : goDoCommand("cmd_scrollTop");
}, 'キャレットをページ先頭へ移動');

key.setCaretKey('J', function (ev) {
    util.getSelectionController().scrollLine(true);
}, '画面を一行分下へスクロール');

key.setCaretKey('K', function (ev) {
    util.getSelectionController().scrollLine(false);
}, '画面を一行分上へスクロール');

key.setCaretKey(',', function (ev) {
    util.getSelectionController().scrollHorizontal(true);
    goDoCommand("cmd_scrollLeft");
}, '左へスクロール');

key.setCaretKey('.', function (ev) {
    goDoCommand("cmd_scrollRight");
    util.getSelectionController().scrollHorizontal(false);
}, '右へスクロール');

key.setCaretKey('z', function (ev) {
    command.recenter(ev);
}, 'キャレットの位置までスクロール');

key.setCaretKey([['C-SPC'], ['C-@']], function (ev) {
    command.setMark(ev);
}, 'マークをセット', true);

key.setCaretKey(':', function (ev, arg) {
    shell.input(null, arg);
}, 'コマンドの実行', true);

key.setCaretKey('R', function (ev) {
    BrowserReload();
}, '更新', true);

key.setCaretKey('B', function (ev) {
    BrowserBack();
}, '戻る');

key.setCaretKey('F', function (ev) {
    BrowserForward();
}, '進む');

key.setCaretKey(['C-x', 'h'], function (ev) {
    goDoCommand("cmd_selectAll");
}, 'すべて選択', true);

key.setCaretKey('f', function (ev) {
    command.focusElement(command.elementsRetrieverTextarea, 0);
}, '最初のインプットエリアへフォーカス', true);

key.setCaretKey('M-p', function (ev) {
    command.walkInputElement(command.elementsRetrieverButton, true, true);
}, '次のボタンへフォーカスを当てる');

key.setCaretKey('M-n', function (ev) {
    command.walkInputElement(command.elementsRetrieverButton, false, true);
}, '前のボタンへフォーカスを当てる');

key.setViewKey('M-r', function (ev) {
    var browser = getBrowser();
    var tabs = browser.mTabContainer.childNodes;
    for (var i = tabs.length - 1; tabs[i] != browser.selectedTab; i--) {
        browser.removeTab(tabs[i]);
    }
}, '右側のタブを全て閉じる', true);

key.setViewKey('M-<up>', function (ev) {
    var uri = getBrowser().currentURI;
    if (uri.path == "/") {
        return;
    }
    var pathList = uri.path.split("/");
    if (!pathList.pop()) {
        pathList.pop();
    }
    loadURI(uri.prePath + pathList.join("/") + ("/"));
}, '一つ上のディレクトリへ移動');


// 2010.06.28 HoK setting ============================================
key.setViewKey('e', function (aEvent, aArg) {
    ext.exec("hok-start-foreground-mode", aArg);
}, 'Hit a Hint を開始', true);

key.setViewKey('E', function (aEvent, aArg) {
    ext.exec("hok-start-background-mode", aArg);
}, 'リンクをバックグラウンドで開く Hit a Hint を開始', true);

key.setViewKey(';', function (aEvent, aArg) {
    ext.exec("hok-start-extended-mode", aArg);
}, 'HoK - 拡張ヒントモード', true);

key.setViewKey(['C-c', 'C-e'], function (aEvent, aArg) {
    ext.exec("hok-start-continuous-mode", aArg);
}, 'リンクを連続して開く Hit a Hint を開始', true);

// 2010.06.28 kungfloo setting ============================================
key.defineKey([key.modes.VIEW, key.modes.CARET], 'r', function (ev, arg) {
    ext.exec("kungfloo-reblog", arg, ev);
}, 'kungfloo - Reblog', true);

// 2012.01.24 Tanything setting ===========================================
key.setViewKey("C-;", function (ev, arg) {
    ext.exec("tanything", arg);
}, "タブを一覧表示", true);

// 2012.01.25 Caret hint setting ==========================================
key.setCaretKey('s', function (ev, arg) {
    ext.exec("swap-caret", arg, ev);
}, 'キャレットを交換', true);

key.setViewKey('i', function (ev, arg) {
    util.setBoolPref("accessibility.browsewithcaret", true);
}, 'キャレットモード', true);

key.setCaretKey('i', function (ev, arg) {
    util.setBoolPref("accessibility.browsewithcaret", false);
}, 'キャレットモードを抜ける', true);

// 2014.05.25 LinkSnail setting ==========================================
// https://github.com/gongo/keysnail_plugin/blob/master/linksnail.ks.js
key.setGlobalKey(['C-c', 't'], function (ev, arg) {
    ext.exec('linksnail', arg, ev);
}, 'LinkSnail', true);

