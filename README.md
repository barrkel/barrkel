Barry's Bash setup
---

Tree layout:

    .
    |-- base               - directory containing platform / environment specific partial profiles
    |   |-- posix
    |   |   |-- bin        - executables
    |   |   |-- home       - files and directories to copy into home; any conflict, first placed wins
    |   |   |-- home.d     - directories that are turned into files to build with sort | xargs cat
    |   |   |   `-- .inputrc/00part - example: a part to build ~/.inputrc out of
    |   |   |-- profile.sh - file to source in ~/.bash_profile
    |   |   `-- rc.sh      - file to source in ~/.bashrc
    |   |-- linux
    |   |-- mono
    |   |-- osx
    |   |-- clr
    |   `-- windows
    |-- config
    |   `-- <hostname>     - contains a list of directories from base to apply
    |-- host
    |   `-- <hostname>     - directory structure mirrors pattern of base/
    |-- setup
    |-- src                - TODO: source for some utilities that need compiling per-arch / per-OS
    |-- cache              - a copy of the home directory as it is expected to be; symlinks are absolute
    `-- staging            - setup builds up the home directory in here


How to use:
---

Create a file config/$(hostname) containing one line per directory in base/ to apply, ordered
from most specific to least specific.

Ensure a valid implementation of _pathlist exists in the bin/ directory of one of the applied profiles. The
existing _pathlist expects GNU grep and sed.


