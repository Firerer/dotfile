# Pi terminal keybindings: Shift+Enter / Alt+Enter

Observed in stack:

```text
gnome-terminal -> zellij -> fish -> pi
```

Pi config is not the problem:

```text
~/.pi/agent/keybindings.json: absent
Pi effective bindings:
  tui.input.newLine      shift+enter
  app.message.followUp   alt+enter
```

Problem: terminal/Zellij key protocol mismatch.

Expected sequences:

```text
Shift+Enter  \x1b[13;2u
Alt+Enter    \x1b[13;3u
```

Likely actual behavior:

```text
Shift+Enter  \r       # indistinguishable from Enter
Alt+Enter    \x1b\r   # legacy ESC+Enter
```

Pi treats legacy `ESC+Enter` (`\x1b\r`) as `shift+enter` when Kitty keyboard protocol is active, so real `Alt+Enter` inserts newline. `Shift+Enter` does not work because it arrives as plain Enter.

Fix options:

1. Prefer Kitty/Ghostty/WezTerm over GNOME Terminal.
2. Test Pi outside Zellij to isolate terminal vs Zellij.
3. Try in `~/.config/zellij/config.kdl`, then fully restart Zellij:

```kdl
support_kitty_keyboard_protocol false
```

This may stop `Alt+Enter` being misread, but `Shift+Enter` still requires terminal support for modified Enter.
