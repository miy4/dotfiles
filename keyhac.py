import sys
import os
import datetime

import pyauto
from keyhac import *


def configure(keymap):

    def on_web_browser(window):
        if window.getProcessName() in ("firefox.exe"):
            return True
        return False

    ### グローバル設定
    keymap_at_work = keymap.defineWindowKeymap()

    # 変換キー
    keymap.replaceKey(28, "RCtrl")
    keymap_at_work["O-RCtrl"] = "(28)"

    # 無変換キー
    #keymap.replaceKey(29, "LAlt")
    #keymap_at_work["O-LAlt"] = "(29)"

    ### Webブラウザ向け設定
    keymap_browser = keymap.defineWindowKeymap(check_func=on_web_browser)
    keymap_browser["C-Period"] = "C-Tab"
    keymap_browser["C-Comma"] = "C-S-Tab"
    keymap_browser["C-P"] = "Up"
    keymap_browser["C-N"] = "Down"
    keymap_browser["C-F"] = "Right"
    keymap_browser["C-b"] = "Left"
    keymap_browser["C-A"] = "Home"
    keymap_browser["C-E"] = "End"
    keymap_browser["A-F"] = "C-Right"
    keymap_browser["A-B"] = "C-Left"
    keymap_browser["C-D"] = "Delete"
    keymap_browser["C-H"] = "Back"
    keymap_browser["C-K"] = "S-End","C-X"
