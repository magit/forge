---
title: 
name: 🪳 Bug report
about: Report a defect. Do not use this for support requests and feature suggestions.
---

Please explain
    (1) what behavior you expected
    (2) what behavior you observed
    (3) and how we can reproduce the issue.

You don't have to quote the above lines to do that.

Please include a backtrace in your report.  In most cases doing:

    M-x toggle-debug-on-error RET

and then going through the steps again should result in a backtrace.

Also post the output of:

    M-x magit-version RET

Before reporting a defect please try to reproduce it using an Emacs instance in which only Magit and its dependencies have been loaded. Other packages or your configuration should not be loaded. This makes it easier to determine whether the issue lays with Magit or something else.

---- now delete this line and everything above ----
