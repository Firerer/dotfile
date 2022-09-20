#!/usr/bin/env python
import sys
import os
from itertools import chain


def expand_dict(opts, category="", command="xdg-open"):
    return {f"{category} . {k}": f"{command} '{v}' & disown" for k, v in opts.items()}


def gen_monash_links(id, start=5, brk=15):
    base = "https://lms.monash.edu/course/view.php"
    home = f"{base}?id={id}"
    records = f"{home}&section=1"
    assess = f"{home}&section=4"
    weeks = {
        f"week{i}": f"{home}&section={v}"
        for i, v in enumerate(chain(range(start, brk), range(brk + 1, 20)))
    }
    return {"homepage": home, "recordings": records, "assessments": assess} | weeks


all = {
    "monash moodle": {
        "base": "https://lms.monash.edu/my/",
        "FIT5137 Adv DB": gen_monash_links("140950"),
    }
}

options = {
    **expand_dict(
        {
            "allocate+": "https://my-timetable.monash.edu/even/student",
            "gmail": "https://mail.google.com/mail/u/0/#inbox",
            "moansh mail": "https://mail.google.com/mail/u/1/#inbox",
            "monash drive": "https://drive.google.com/drive/u/1/",
            "monash moodle": "https://lms.monash.edu/my/",
            "monash gitlab": "https://git.infotech.monash.edu/fit5042/fit5042-s2-2022/dliu0024",
            "qmk": "https://config.qmk.fm/#/ergodox_ez/glow/LAYOUT_ergodox_pretty",
            "spotify": "https://open.spotify.com/",
            "twitch": "https://www.twitch.tv/",
            "twitter": "https://twitter.com/home",
            "wes": "https://my.monash.edu/wes/",
            "yinwang": "https://www.yinwang.org/",
            "youtube": "https://www.youtube.com/",
        }
    ),
    **expand_dict(gen_monash_links("140950"), "FIT5137 Advanced database technology"),
    **expand_dict(gen_monash_links("140923"), "FIT5042 Enterprise application"),
    **expand_dict(
        {"home": "https://lms.monash.edu/course/view.php?id=135542"},
        "Master and Honours Thesis",
    ),
}

if len(sys.argv) > 1:
    opt = sys.argv[1]
    command = options[opt]
    with open("/tmp/debug", "a+") as f:
        print(command, file=f)
    os.system(command)
    exit(0)
else:
    for opt in options:
        print(opt)
        # print(opt, options[opt])
