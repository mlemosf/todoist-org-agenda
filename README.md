
# Table of Contents

1.  [Description](#org97d2575)
    1.  [Maintainers](#org718db5a)
2.  [Prerequisites](#org99d657f)
3.  [Installation](#org7486642)
    1.  [Execution](#orgf0fe187)



<a id="org97d2575"></a>

# Description

todoist-org-agenda is a simple library writen in emacs lisp that calls the REST API for the Todoist app writes your tasks to an org file readable by org-agenda.
It&rsquo;s a pure elisp module inspired by ****Danesprite/todoist-org-mode****, but this program is intended to be called from inside emacs, without the need for an external application.


<a id="org718db5a"></a>

## Maintainers

-   @mlemosf (Author)


<a id="org99d657f"></a>

# Prerequisites

This module has no prerequisites.


<a id="org7486642"></a>

# Installation

Just download the ****todoist-org-agenda.el**** file and store it where you want it. To load all functions, just call load-file from the command bar.

    M-x load-file FILE-NAME RET

After download, add an entry to your authinfo file (e.g. ~/.authinfo.gpg) in the form

    machine api.todoist.com password TOKEN

where TOKEN is your Todoist API token, which can be found at <https://todoist.com/prefs/integrations>.


<a id="orgf0fe187"></a>

## Execution

To execute the file just run:

    M-x mlemosf/get-todoist-tasks RET

