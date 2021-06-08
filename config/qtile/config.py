# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

from typing import List  # noqa: F401

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Screen, ScratchPad, DropDown
from libqtile.lazy import lazy
from libqtile import hook

mod = "mod4"

groups = [
    Group("VIM"),
    Group("Visualize"),
    Group("VS Code"),
    Group("BG"),
	ScratchPad("scratchpad", [
		DropDown("python", "alacritty -e python"),
		DropDown("vpn", "alacritty -e globalprotect")
	])
]

keys = [
    # Switch between windows in current stack pane
    Key([mod], "k", lazy.layout.down(), desc="Move focus down in stack pane"),
    Key([mod], "j", lazy.layout.up(), desc="Move focus up in stack pane"),
    Key([mod], "l", lazy.layout.next(), desc="Move focus to next stack"),
    Key([mod], "m", lazy.layout.maximize(), desc="Maximize window in stack"),
    Key([mod], "n", lazy.layout.normalize(), desc="Normalize window stack"),
    Key([mod, "shift"], "space", lazy.layout.flip(), desc="Swap the sizes of MonadTall"),
    Key([mod], "Return", lazy.layout.swap_main(), desc="Swap the main pane"),

    # Move windows up or down in current stack
    Key([mod, "control"], "k", lazy.layout.shuffle_down(), desc="Move window down in current stack "),
    Key([mod, "control"], "j", lazy.layout.shuffle_up(), desc="Move window up in current stack "),
    Key([mod, "control"], "l", lazy.layout.client_to_next(), desc="Move window to next stack "),
    Key([mod, "control"], "m", lazy.layout.grow(), desc="Grow the window "),
    Key([mod, "control"], "n", lazy.layout.shrink(), desc="Shrink the window "),

    # Swap panes of split stack
    Key([mod, "shift"], "space", lazy.layout.flip(), desc="Swap panes of split stack"),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    # Key([mod], "Return", lazy.layout.toggle_split(), desc="Toggle between split and unsplit sides of stack"),

    # Toggle between different layouts as defined below
    Key([mod], "space", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "w", lazy.window.kill(), desc="Kill focused window"),
    Key([mod], "t", lazy.window.toggle_floating(), desc="Toggle floating mode"),

    # QTile life cycle
    Key([mod, "control"], "r", lazy.restart(), desc="Restart qtile"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown qtile"),

    # Emulate XMonad screen switching (Mod + number)
    *[Key([mod], str(i+1), lazy.group[g.name].toscreen(),
		    desc="Switch to group {}".format(g.name)
	 ) for i, g in enumerate(groups)],

    # Emulate XMonad send window to another screen
    *[Key([mod, "shift"], str(i+1), lazy.window.togroup(g.name),
		    desc="move focused window to group {}".format(g.name)
	 ) for i, g in enumerate(groups)],

    # Scratchpad
    Key([mod], 'F9', lazy.group['scratchpad'].dropdown_toggle('python')),
    Key([mod], 'F10', lazy.group['scratchpad'].dropdown_toggle('vpn')),

    # Application launches
    Key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget"),
    Key([mod, "shift"], "Return", lazy.spawn('alacritty'), desc="Launch terminal"),
    Key([mod, "shift"], "c", lazy.spawn('chromium-browser'), desc="Launch Chrome"),
    Key([mod, "shift"], "g", lazy.spawn('code'), desc="Launch VSCode")
]

layouts = [
    layout.MonadTall(
        margin=8,
        border_width=2,
        border_focus='lightgray',
        border_normal='darkgray'
    ),
    layout.Max(),
    layout.VerticalTile(
        margin=8,
        border_width=2,
        border_focus='lightgray',
        border_normal='darkgray'
    )
]

widget_defaults = dict(
    font='sans',
    fontsize=26,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    font="Ubuntu Bold",
                    fontsize=18,
                    padding_x = 20,
                    padding_y = 5,
                    highlight_method = "block",
                    rounded=False,
                    ),
                widget.CurrentLayout(),
                widget.WindowName(fmt="{:10}"),
                widget.TextBox("Press &lt;M-r&gt; to spawn", foreground="#d75f5f"),
                widget.Systray(padding=5),
                widget.CPUGraph(border_width=1, margin_y=0, line_width=2),
                widget.TextBox(text="mem:"),
                widget.MemoryGraph(border_width=1, margin_y=0, line_width=2),
                widget.Clock(format='%Y-%m-%d %a %I:%M %p'),
                widget.CPU(),
            ],
            32,
        ),
    ),
    Screen(
        bottom=bar.Bar(
            [
                widget.GroupBox(
                    font="Ubuntu Bold",
                    fontsize=18,
                    padding_x = 20,
                    padding_y = 5,
                    highlight_method = "block",
                    rounded=False,
                    ),
                widget.CurrentLayout(),
                widget.WindowName(fmt="{:10}"),
                widget.TextBox("Press &lt;M-r&gt; to spawn", foreground="#d75f5f"),
                widget.Systray(padding=5),
                widget.CPUGraph(border_width=1, margin_y=0, line_width=2),
                widget.TextBox(text="mem:"),
                widget.MemoryGraph(border_width=1, margin_y=0, line_width=2),
                widget.Clock(format='%Y-%m-%d %a %I:%M %p'),
                widget.CPU(),
            ],
            64,
        ),
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None  # WARNING: this is deprecated and will be removed soon
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    {'role': 'pop-up'},
    {'wm_type': 'modal'},
    {'wm_type': 'dialog'},
    {'wmclass': 'kazam'}, # Kazam is a video capture tool
    {'wname': 'kazam'},
    {'wmclass': 'confirm'},
    {'wmclass': 'dialog'},
    {'wmclass': 'download'},
    {'wmclass': 'error'},
    {'wmclass': 'matplotlib'},
    {'wmclass': 'Open3D'},
    {'wmclass': 'python3.8'},
    {'wmclass': 'file_progress'},
    {'wmclass': 'notification'},
    {'wmclass': 'splash'},
    {'wmclass': 'toolbar'},
    {'wmclass': 'confirmreset'},  # gitk
    {'wmclass': 'makebranch'},  # gitk
    {'wmclass': 'maketag'},  # gitk
    {'wname': 'branchdialog'},  # gitk
    {'wname': 'pinentry'},  # GPG key password entry
    {'wmclass': 'ssh-askpass'},  # ssh-askpass
])
auto_fullscreen = True
# focus_on_window_activation = "smart"

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
