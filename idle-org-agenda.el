;;; idle-org-agenda.el --- A package that shows your agenda when Emacs is idle

;; Copyright (C) 2010 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Maintainer: Enis Özgen <mail@enisozgen.com>
;; Created: 18 Mar 2010
;; Modified: 24 Dec 2018
;; Version: 1.0
;; Keywords: org, org-mode, org-agenda, calendar
;; URL: https://github.com/enisozgen/idle-org-agenda

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;  Basically, if you don't touch Emacs `idle-org-agenda' will display your
;;  org-agenda after certain time.
;;  That can be useful to remember tasks after come back to work.
;;  This project is comes from John Wiegley's mail at the gmane mailing list
;;  http://article.gmane.org/gmane.emacs.orgmode/23047
;;
;; Please see README.md from the same repository for documentation.

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org-agenda)

(defgroup idle-org nil
  "Tool that displays org-agenda when emacs is idle"
  :group 'org-agenda)

(defcustom idle-org-agenda-interval 300
  "Period of idle time period to run functions"
  :type 'integer
  :group 'idle-org-agenda)

(defcustom idle-org-agenda-key "a"
  "org-agenda key to choose spesific agenda"
  :type 'string
  :group 'idle-org-agenda)

(defun jump-to-idle-org-agenda ()
  "Main functions shows your defined agenda"
  (let ((buf (get-buffer org-agenda-buffer-name))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer))
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer))))
      (org-agenda nil idle-org-agenda-key))))

(defun stop-idle-org-agenda ()
  "Start idle-org-agenda"
  (cancel-function-timers 'jump-to-idle-org-agenda))

(defun start-idle-org-agenda ()
  "Stop idle-org-agenda"
  (run-with-idle-timer idle-org-agenda-interval  t 'jump-to-idle-org-agenda))

(define-minor-mode idle-org-agenda-mode
  "Toggle `idle-org-agenda-mode'

When it active. If you don't touch emacs, after certain time emacs will show
your org-agenda"
  nil
  :lighter ""
  :global t
  (if idle-org-agenda-mode
      (start-idle-org-agenda)
    (stop-idle-org-agenda)))


(provide 'idle-org-agenda)

;;; idle-org-agenda.el ends here
