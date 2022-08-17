cd ~/qmk_firmware/keyboards/ergodox_ez/keymaps/firerrd
qmk json2c firerrd.json -o keymap.c && qmk flash
