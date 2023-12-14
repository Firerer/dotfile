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
            "my unimelb": "https://my.unimelb.edu.au/",
            "lms": "https://canvas.lms.unimelb.edu.au/",
            "IT Internship Portal": "https://careersonline.unimelb.edu.au/s/engineering--it-internship-portal",
            "timetable": "https://mytimetable.students.unimelb.edu.au/odd/student?ss=5ad1c6f026d64713b03d4c4a48d7c6ae",
            "coverletter": "https://docs.google.com/document/u/1/",
            "COMP90043_sec": {
                "home": "https://canvas.lms.unimelb.edu.au/courses/155575",
            },
            "COMP90025_core": {
                "home": "https://canvas.lms.unimelb.edu.au/courses/154487",
            },
            "COMP90082_proj": {
                "home": "https://canvas.lms.unimelb.edu.au/courses/156871",
                "confluence": "https://confluence.cis.unimelb.edu.au:8443/display/COMP900822023SM2GRBlueRing/",
                "repo": "https://github.com/COMP90082-2023-SM2/GR-Bluering",
                "slack": "https://app.slack.com/client/T05K9RMH5NJ/C05KGCA452P",
                "supabase": "https://supabase.com/dashboard/project/mgoqwxgxwuitrtebksfd",
            },
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
