# -*- coding: utf-8 -*-

import re
from xkeysnail.transform import *

# Global modmap (keycode translation)
#define_modmap({
#    Key.CAPSLOCK: Key.LEFT_CTRL
#})

# Multipurpose modmap (multi-key translation)
define_multipurpose_modmap({
    Key.CAPSLOCK: [Key.ESC, Key.LEFT_CTRL],
    Key.MUHENKAN: [Key.MUHENKAN, Key.LEFT_ALT],
    Key.HENKAN: [Key.HENKAN, Key.RIGHT_CTRL]
})

# Keybindings for Firefox/Chrome
define_keymap(re.compile("Waterfox|Firefox|Google-chrome"), {
    # Ctrl+./, to switch next/previous tab
    K("C-dot"): K("C-TAB"),
    K("C-comma"): K("C-Shift-TAB"),
    # Type C-j to focus to the content
    K("C-j"): K("C-f6"),
}, "Firefox and Chrome")

# Keybindings for Emacs
define_keymap(re.compile("Emacs"), {
    K("ESC"): [K("MUHENKAN"), K("ESC")],
    K("C-semicolon"): [K("MUHENKAN"), K("C-semicolon")]
}, "Emacs")

