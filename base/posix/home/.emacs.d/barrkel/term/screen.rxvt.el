;;; screen.rxvt.el --- handle rxvt inside screen like rxvt  -*- lexical-binding: t -*-

(load "term/rxvt")

(defun terminal-init-screen.rxvt ()
  "Terminal initialization function for screen inside rxvt"
  (terminal-init-rxvt))
