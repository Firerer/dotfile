#!/usr/bin/env python
import sys
import os
from itertools import chain


def dict_add_cmd(opts, category="", command="xdg-open"):
    return {f"{category} . {k}": f"{command} '{v}' & disown" for k, v in opts.items()}


options = {
    **dict_add_cmd(
        {
            "puppeteer doc": "https://pptr.dev/api",
            "my unimelb": "https://my.unimelb.edu.au/",
            "Advanced Database Systems (COMP90050)": "https://canvas.lms.unimelb.edu.au/courses/151276",
            "Cluster and Cloud Computing (COMP90024)": "https://canvas.lms.unimelb.edu.au/courses/151783",
            "Distributed Systems (COMP90015)": "https://canvas.lms.unimelb.edu.au/courses/151445",
            "Software Processes and Management (SWEN90016)": "https://canvas.lms.unimelb.edu.au/courses/153205",
            "unimelb dashboard": "https://lms.monash.edu/my/",
            "timetable": "https://mytimetable.students.unimelb.edu.au/odd/student?ss=5ad1c6f026d64713b03d4c4a48d7c6ae",
            "unimelb course manage": "https://unimelb.t1cloud.com/T1Default/CiAnywhere/Web/UNIMELB/LogOn/ESTUDENT?webadf=true&returnUrl=https%3a%2f%2funimelb-web.t1cloud.com%2fT1SMDefault%2fWebApps%2feStudent%2fSM%2fPersDtls10.aspx%3fr%3d%2523UM.STUDENT.APPLICANT%26f%3d%24S1.EST.PERSDTLS.WEB%26ciredirect%3d1",
            "qmk": "https://config.qmk.fm/#/ergodox_ez/glow/LAYOUT_ergodox_pretty",
            "yinwang": "https://www.yinwang.org/",
        }
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
