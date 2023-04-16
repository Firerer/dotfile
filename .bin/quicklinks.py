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
            "ai search": "https://www.perplexity.ai",
            "Advanced Database Systems (COMP90050)": {
                "": "https://canvas.lms.unimelb.edu.au/courses/151276",
                "lectures files": "https://canvas.lms.unimelb.edu.au/courses/151276/pages/lectures-slides?module_item_id=4582954",
                "tuts files": "https://canvas.lms.unimelb.edu.au/courses/151276/pages/tutorial-info-per-week?module_item_id=4582955",
                "live": "https://canvas.lms.unimelb.edu.au/courses/151276/external_tools/701",
            },
            "Cluster and Cloud Computing (COMP90024)": {
                "": "https://canvas.lms.unimelb.edu.au/courses/151783",
                "Australian Digital Observatory": "https://www.digitalobservatory.net.au/",
                "videos": "https://canvas.lms.unimelb.edu.au/courses/151783#context_module_834202",
                "live/records": "https://canvas.lms.unimelb.edu.au/courses/151783/external_tools/701",
                "Karaage": "https://dashboard.hpc.unimelb.edu.au/karaage/",
                "a2 teams": "https://docs.google.com/spreadsheets/d/1OmI1uf4ZSqZEpFffmFkmOBpX-Rw9gSCw5jQj-jY98vs/edit#gid=0",
            },
            "Distributed Systems (COMP90015)": {
                "": "https://canvas.lms.unimelb.edu.au/courses/151445",
                "lectures": "https://canvas.lms.unimelb.edu.au/courses/151445/external_tools/701",
                "files": "https://canvas.lms.unimelb.edu.au/courses/151445/modules",
                "quizzes": "https://canvas.lms.unimelb.edu.au/courses/151445/quizzes",
            },
            "Software Processes and Management (SWEN90016)": {
                "": "https://canvas.lms.unimelb.edu.au/courses/153205",
                "zoom": "https://applications.zoom.us/lti/rich/j/88635663721?oauth_consumer_key=An-rf1gZRTu-IFz3CIlUnA&x_zm_session_id_token=eyJ0eXAiOiJKV1QiLCJzdiI6IjAwMDAwMSIsInptX3NrbSI6InptX28ybSIsImFsZyI6IkhTMjU2In0.eyJhdWQiOiJpbnRlZ3JhdGlvbiIsImlzcyI6ImludGVncmF0aW9uIiwiZXhwIjoxNjc5NDcyMTAwLCJpYXQiOjE2Nzk0NzE1MDAsImp0aSI6Ijc4YWUyNTQ5LTJjMDEtNGQwMi1hMThkLTI4NTZkM2Y1ZGYyYyJ9.-zjp17aknA5N6t-ok4yNN5UrXHPGuM9KtUh4uGAk0ts&lti_scid=5b3faacaa78fe6818285309389546298d09f1ce201aa5f6e99a59f3f8683dcb8",
                "files": "https://canvas.lms.unimelb.edu.au/courses/153205/modules",
                "quizzes": "https://canvas.lms.unimelb.edu.au/courses/153205/quizzes",
                "a2 doc": "https://docs.google.com/document/d/1uw0iwM1wYXWg7h--dzRhiBxsxbkKpRny9WKUjJwwwxs/edit#heading=h.6i03fy14yfsq",
            },
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
