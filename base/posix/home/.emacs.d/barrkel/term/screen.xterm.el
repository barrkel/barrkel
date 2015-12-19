;;; screen.xterm.el --- handle xterm inside screen like xterm  -*- lexical-binding: t -*-

(load "term/xterm")

(defun terminal-init-screen.xterm ()
  "Terminal initialization function for screen inside xterm"
  (terminal-init-xterm))
