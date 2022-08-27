#!/usr/bin/python
import sys
import os
from itertools import chain


def expand_dict(opts, category="", command="xdg-open"):
    return {f"{k} {category}": f"{command} '{v}' & disown" for k, v in opts.items()}


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


options = {
    **expand_dict(
        {
            "monash moodle": "https://lms.monash.edu/my/",
            "wes": "https://my.monash.edu/wes/",
            "allocate+": "https://my-timetable.monash.edu/even/student",
            "twitch": "https://www.twitch.tv/",
            "youtube": "https://www.youtube.com/",
            "twitter": "https://twitter.com/home",
            "yinwang": "https://www.yinwang.org/"
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
