Barry's Bash setup
---

Tree layout:

    .
    |-- base
    |   |-- posix
    |   |   |-- bin        - executables
    |   |   |-- home       - files and directories to symlink into home; any conflict, first placed wins
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
    |-- src                - source for some utilities that need compiling per-arch / per-OS
    |-- cache              - a copy of the home directory as it is expected to be; symlinks are absolute
    `-- staging            - setup builds up the home directory in here


