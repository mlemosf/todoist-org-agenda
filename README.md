
# Table of Contents

1.  [Description](#orga081957)
    1.  [Maintainers](#orgad28f68)
2.  [Prerequisites](#org69af932)
3.  [Installation](#org7bbc3f3)
4.  [Functions](#org4b7fef6)
    1.  [Getting tasks](#orgd0e7064)
    2.  [Closing tasks](#org6028f07)



<a id="orga081957"></a>

# Description

todoist-org-agenda is a simple library writen in emacs lisp that calls the REST API for the Todoist app writes your tasks to an org file readable by org-agenda.
It&rsquo;s a pure elisp module inspired by ****Danesprite/todoist-org-mode****, but this program is intended to be called from inside emacs, without the need for an external application.
****todoist-org-agenda is NOT an official Doist product****.


<a id="orgad28f68"></a>

## Maintainers

-   @mlemosf (Author)


<a id="org69af932"></a>

# Prerequisites

This module has no prerequisites.


<a id="org7bbc3f3"></a>

# Installation

Clone the repository to an **etc** folder inside your emacs.d.

	git clone https://github.com/mlemosf/todoist-org-agenda.git ~/.emacs.d/etc/todoist-org-agenda

To load all functions, just call load-file from the command bar.

    M-x load-file FILE-NAME RET

After download, add an entry to your authinfo file (e.g. ~/.authinfo.gpg) in the form

    machine api.todoist.com password TOKEN

where TOKEN is your Todoist API token, which can be found at <https://todoist.com/prefs/integrations>.


<a id="org4b7fef6"></a>

# Functions


<a id="orgd0e7064"></a>

## Getting tasks

For retrieving tasks, just run:

    M-x mlemosf/todoist/get-todoist-tasks RET


<a id="org6028f07"></a>

## Closing tasks

To close your DONE tasks, access your todoist org file (by default, the file should stay at ****~/.config/org/todoist.org****) and run:

    M-x mlemosf/todoist/close-done-tasks RET

The function will parse the org-file and close all tasks mark as DONE by making a POST request to the Todoist API.

