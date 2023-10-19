local wezterm = require("wezterm")
local act = wezterm.action

local config = {}

if wezterm.config_builder then
    config = wezterm.config_builder()
end

-- Fonts ------------------------------------------------------------
config.font = wezterm.font_with_fallback({
    { family = "Cica" },
    { family = "Cica", assume_emoji_presentation = true },
})
config.window_frame = {
    font = wezterm.font { family = "Roboto", weight = "Bold" },
}

-- Colors -----------------------------------------------------------
config.color_scheme = "ChallengerDeep"

local challenger_deep = wezterm.get_builtin_color_schemes()["ChallengerDeep"]
config.colors = {
    tab_bar = {
        active_tab = {
            bg_color = challenger_deep.background,
            fg_color = challenger_deep.brights[1],
        },
        inactive_tab_hover = {
            bg_color = challenger_deep.ansi[6],
            fg_color = challenger_deep.brights[1],
        },
    },
}

-- Key tables -------------------------------------------------------
--config.leader = { key = "t", mods = "CTRL" }
--config.keys = {
--    { key = "1",        mods = "LEADER",      action = act.ActivateTab(0) },
--    { key = "2",        mods = "LEADER",      action = act.ActivateTab(1) },
--    { key = "3",        mods = "LEADER",      action = act.ActivateTab(2) },
--    { key = "4",        mods = "LEADER",      action = act.ActivateTab(3) },
--    { key = "5",        mods = "LEADER",      action = act.ActivateTab(4) },
--    { key = "6",        mods = "LEADER",      action = act.ActivateTab(5) },
--    { key = "7",        mods = "LEADER",      action = act.ActivateTab(6) },
--    { key = "8",        mods = "LEADER",      action = act.ActivateTab(7) },
--    { key = "9",        mods = "LEADER",      action = act.ActivateTab(8) },
--    { key = "p",        mods = "LEADER",      action = act.ActivateTabRelative(-1) },
--    { key = ",",        mods = "LEADER",      action = act.ActivateTabRelative(-1) },
--    { key = "n",        mods = "LEADER",      action = act.ActivateTabRelative(1) },
--    { key = ".",        mods = "LEADER",      action = act.ActivateTabRelative(1) },
--    { key = "t",        mods = "LEADER|CTRL", action = act.ActivateLastTab },
--    { key = "w",        mods = "LEADER",      action = act.PaneSelect },
--    { key = "-",        mods = "LEADER",      action = act.SplitVertical({ domain = "CurrentPaneDomain" }) },
--    { key = "\\",       mods = "LEADER",      action = act.SplitHorizontal({ domain = "CurrentPaneDomain" }) },
--    { key = "c",        mods = "LEADER",      action = act.SpawnTab("CurrentPaneDomain") },
--    { key = "k",        mods = "LEADER",      action = act.CloseCurrentTab({ confirm = false }) },
--    { key = "PageUp",   mods = "LEADER",      action = act.MoveTabRelative(-1) },
--    { key = "PageDown", mods = "LEADER",      action = act.MoveTabRelative(1) },
--    { key = "[",        mods = "LEADER",      action = wezterm.action.ActivateCopyMode },
--}

-- For Windows ------------------------------------------------------
local function contains_distro_in_wsl_domains(distro)
    for _, domain in ipairs(wezterm.default_wsl_domains()) do
        if domain.name == distro then
            return true
        end
    end
    return false
end

--local function spawn_action_callback(action, base_dir)
--    return function(window, pane)
--        local spawnCommand = { domain = "CurrentPaneDomain" }
--        --local pwd = os.getenv("PWD")
--        --if pwd then
--        --    spawnCommand.cwd = base_dir .. pwd
--        --end
--        if base_dir then
--            spawnCommand.cwd = base_dir
--        end
--
--        window:perform_action(action(spawnCommand), pane)
--    end
--end

if wezterm.target_triple == "x86_64-pc-windows-msvc" then
    config.launch_menu = {
        { label = "PowerShell", args = { "pwsh.exe", "--NoLogo" } }
    }

    if contains_distro_in_wsl_domains("WSL:Arch") then
        config.default_prog = { "wsl.exe", "--distribution", "Arch" }
        config.default_cwd = "//wsl$/Arch/home/miy4"

        --local split_vertical = spawn_action_callback(act.SplitVertical, "//wsl$/Arch/home/miy4")
        --local split_horizontal = spawn_action_callback(act.SplitHorizontal, "//wsl$/Arch/home/miy4")

        --table.insert(config.keys, { key = "-", mods = "LEADER", action = wezterm.action_callback(split_vertical) })
        --table.insert(config.keys, { key = "\\", mods = "LEADER", action = wezterm.action_callback(split_horizontal) })
    end
end

return config
