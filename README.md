
# Table of Contents

1.  [Description](#org7af494c)
    1.  [Maintainers](#org9548906)
2.  [Prerequisites](#org887482d)
3.  [Configuraton](#org0da39ca)
    1.  [Execution](#org3b3b241)



<a id="org7af494c"></a>

# Description

todoist-org-agenda is a simple library listen in elisp that gets the list of tasks from the webapp Todoist and writes them to an org file readable by org-agenda.
It&rsquo;s a pure elisp module inspired by (Danesprite/todoist-org-mode), intended to be called from inside emacs


<a id="org9548906"></a>

## Maintainers

-   @mlemosf (Author)


<a id="org887482d"></a>

# Prerequisites

This module has no prerequisites.


<a id="org0da39ca"></a>

# Configuraton

Just download the ****todoist-org-agenda.el**** file and require it on your main emacs configuration file to use the functions.

After download, add an entry to your authinfo file (e.g. ~/.authinfo.gpg) in the form

    machine api.todoist.com password TOKEN

where token is your Todoist API token, which can be found at <https://todoist.com/prefs/integrations>


<a id="org3b3b241"></a>

## Execution

To execute the file just run:

    M-x mlemosf/get-todoist-tasks RET

