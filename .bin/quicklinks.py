#!/usr/bin/env python
from os import system
from sys import argv


def flat_dict(dictionary, prefix: str = ""):
    for k, v in dictionary.items():
        new_key = k if prefix == "" else f"{prefix} / {k}"
        if not isinstance(v, str):
            yield from flat_dict(v, prefix=new_key)
        else:
            yield new_key, f"xdg-open '{v}' & disown"


options = dict(
    flat_dict(
        {
            "puppeteer doc": "https://pptr.dev/api",
            "my unimelb": "https://my.unimelb.edu.au/",
            "Advanced Database Systems (COMP90050)": {
                "": "https://canvas.lms.unimelb.edu.au/courses/151276",
                "lectures files": "https://canvas.lms.unimelb.edu.au/courses/151276/pages/lectures-slides?module_item_id=4582954",
                "tuts files": "https://canvas.lms.unimelb.edu.au/courses/151276/pages/tutorial-info-per-week?module_item_id=4582955",
            },
            "Cluster and Cloud Computing (COMP90024)": {
                "": "https://canvas.lms.unimelb.edu.au/courses/151783",
            },
            "Distributed Systems (COMP90015)": {
                "": "https://canvas.lms.unimelb.edu.au/courses/151445",
                "files": "https://canvas.lms.unimelb.edu.au/courses/151445/modules",
                "quizzes": "https://canvas.lms.unimelb.edu.au/courses/151445/quizzes",
            },
            "Software Processes and Management (SWEN90016)": {
                "": "https://canvas.lms.unimelb.edu.au/courses/153205",
                "files": "https://canvas.lms.unimelb.edu.au/courses/153205/modules",
                "quizzes": "https://canvas.lms.unimelb.edu.au/courses/153205/quizzes",
            },
            "unimelb dashboard": "https://lms.monash.edu/my/",
            "timetable": "https://mytimetable.students.unimelb.edu.au/odd/student?ss=5ad1c6f026d64713b03d4c4a48d7c6ae",
            "unimelb course manage": "https://unimelb.t1cloud.com/T1Default/CiAnywhere/Web/UNIMELB/LogOn/ESTUDENT?webadf=true&returnUrl=https%3a%2f%2funimelb-web.t1cloud.com%2fT1SMDefault%2fWebApps%2feStudent%2fSM%2fPersDtls10.aspx%3fr%3d%2523UM.STUDENT.APPLICANT%26f%3d%24S1.EST.PERSDTLS.WEB%26ciredirect%3d1",
            "qmk": "https://config.qmk.fm/#/ergodox_ez/glow/LAYOUT_ergodox_pretty",
            "yinwang": "https://www.yinwang.org/",
        }
    )
)
if len(argv) > 1:
    system(options[argv[1]])
else:
    for opt in options:
        print(opt)
