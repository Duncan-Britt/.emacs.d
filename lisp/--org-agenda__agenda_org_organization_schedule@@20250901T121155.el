;;; --org-agenda__agenda_org_organization_schedule@@20250901T121155.el --- org-agenda -*- lexical-binding: t -*-

;;; Commentary:
;; title: org-agenda
;; keywords: :agenda:org:organization:schedule:
;; date: [2025-09-01 Mon 12:11]
;; identifier: 20250901T121155

;;; Code:
(use-package org
  :ensure nil
  :config
  (setq org-agenda-files (list (expand-file-name "~/Dropbox/agenda/agenda.org")))
  ;; (setq org-archive-location "~/Dropbox/agenda/agenda_archive.org::%s_archive") ;; <-- unused? Org Archiver has it's own location.
  (with-eval-after-load 'org-agenda
    ;; I don't like how Emacs, by default, shows me upcoming deadlines
    ;; for tasks which are not yet actionable, so I'm modifying
    ;; `org-agenda-get-deadlines' to not present me with deadlines prior
    ;; to the day the task is scheduled, if it is scheduled.
    (defun org-agenda-get-deadlines (&optional with-hour)
      "Return the deadline information for agenda display.
When WITH-HOUR is non-nil, only return deadlines with an hour
specification like [h]h:mm."
      (with-no-warnings (defvar date))
      (let* ((props (list 'mouse-face 'highlight
		          'org-not-done-regexp org-not-done-regexp
		          'org-todo-regexp org-todo-regexp
		          'org-complex-heading-regexp org-complex-heading-regexp
		          'help-echo
		          (format "mouse-2 or RET jump to org file %s"
			          (abbreviate-file-name buffer-file-name))))
	     (regexp (if with-hour
		         org-deadline-time-hour-regexp
		       org-deadline-time-regexp))
	     (today (org-today))
	     (today? (org-agenda-today-p date)) ; DATE bound by calendar.
	     (current (calendar-absolute-from-gregorian date))
	     deadline-items)
        (goto-char (point-min))
        (if (org-element--cache-active-p)
            (org-element-cache-map
             (lambda (el)
               (when (and (org-element-property :deadline el)
                          ;; Only consider active timestamp values.
                          (memq (org-element-property
                                 :type
                                 (org-element-property :deadline el))
                                '(diary active active-range))
                          (or (not with-hour)
                              (org-element-property
                               :hour-start
                               (org-element-property :deadline el))
                              (org-element-property
                               :hour-end
                               (org-element-property :deadline el)))
                          ;; START lines added by me ===============
                          ;; Accept only deadlined tasks which are either not scheduled, or scheduled prior to now
                          (let ((scheduled (org-element-property :scheduled el)))
                            (or (not scheduled)
                                (let ((sched-date
                                       (calendar-absolute-from-gregorian (list (org-element-property :month-start scheduled)
                                                                               (org-element-property :day-start scheduled)
                                                                               (org-element-property :year-start scheduled)))))
                                  (<= sched-date current)))))
                 ;; END lines added by me ==========================
                 (goto-char (org-element-property :contents-begin el))
                 (catch :skip
	           (org-agenda-skip el)
	           (let* ((s (substring (org-element-property
                                         :raw-value
                                         (org-element-property :deadline el))
                                        1 -1))
	                  (pos (save-excursion
                                 (goto-char (org-element-property :contents-begin el))
                                 ;; We intentionally leave NOERROR
                                 ;; argument in `re-search-forward' nil.  If
                                 ;; the search fails here, something went
                                 ;; wrong and we are looking at
                                 ;; non-matching headline.
                                 (re-search-forward regexp (line-end-position))
                                 (1- (match-beginning 1))))
	                  (todo-state (org-element-property :todo-keyword el))
	                  (done? (eq 'done (org-element-property :todo-type el)))
                          (sexp? (eq 'diary
                                     (org-element-property
                                      :type (org-element-property :deadline el))))
	                  ;; DEADLINE is the deadline date for the entry.  It is
	                  ;; either the base date or the last repeat, according
	                  ;; to `org-agenda-prefer-last-repeat'.
	                  (deadline
		           (cond
		            (sexp? (org-agenda--timestamp-to-absolute s current))
		            ((or (eq org-agenda-prefer-last-repeat t)
		                 (member todo-state org-agenda-prefer-last-repeat))
		             (org-agenda--timestamp-to-absolute
		              s today 'past (current-buffer) pos))
		            (t (org-agenda--timestamp-to-absolute s))))
	                  ;; REPEAT is the future repeat closest from CURRENT,
	                  ;; according to `org-agenda-show-future-repeats'. If
	                  ;; the latter is nil, or if the time stamp has no
	                  ;; repeat part, default to DEADLINE.
	                  (repeat
		           (cond
		            (sexp? deadline)
		            ((<= current today) deadline)
		            ((not org-agenda-show-future-repeats) deadline)
		            (t
		             (let ((base (if (eq org-agenda-show-future-repeats 'next)
				             (1+ today)
				           current)))
		               (org-agenda--timestamp-to-absolute
		                s base 'future (current-buffer) pos)))))
	                  (diff (- deadline current))
	                  (suppress-prewarning
		           (let ((scheduled
		                  (and org-agenda-skip-deadline-prewarning-if-scheduled
                                       (org-element-property
                                        :raw-value
                                        (org-element-property :scheduled el)))))
		             (cond
		              ((not scheduled) nil)
		              ;; The current item has a scheduled date, so
		              ;; evaluate its prewarning lead time.
		              ((integerp org-agenda-skip-deadline-prewarning-if-scheduled)
		               ;; Use global prewarning-restart lead time.
		               org-agenda-skip-deadline-prewarning-if-scheduled)
		              ((eq org-agenda-skip-deadline-prewarning-if-scheduled
			           'pre-scheduled)
		               ;; Set pre-warning to no earlier than SCHEDULED.
		               (min (- deadline
			               (org-agenda--timestamp-to-absolute scheduled))
			            org-deadline-warning-days))
		              ;; Set pre-warning to deadline.
		              (t 0))))
	                  (wdays (or suppress-prewarning (org-get-wdays s))))
	             (cond
	              ;; Only display deadlines at their base date, at future
	              ;; repeat occurrences or in today agenda.
	              ((= current deadline) nil)
	              ((= current repeat) nil)
	              ((not today?) (throw :skip nil))
	              ;; Upcoming deadline: display within warning period WDAYS.
	              ((> deadline current) (when (> diff wdays) (throw :skip nil)))
	              ;; Overdue deadline: warn about it for
	              ;; `org-deadline-past-days' duration.
	              (t (when (< org-deadline-past-days (- diff)) (throw :skip nil))))
	             ;; Possibly skip done tasks.
	             (when (and done?
		                (or org-agenda-skip-deadline-if-done
			            (/= deadline current)))
	               (throw :skip nil))
	             (save-excursion
                       (goto-char (org-element-property :begin el))
	               (let* ((category (org-get-category))
                              (effort (save-match-data (or (get-text-property (point) 'effort)
                                                           (org-element-property (intern (concat ":" (upcase org-effort-property))) el))))
                              (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
		              (level (make-string (org-element-property :level el)
				                  ?\s))
		              (head (save-excursion
                                      (goto-char (org-element-property :begin el))
                                      (re-search-forward org-outline-regexp-bol)
                                      (buffer-substring-no-properties (point) (line-end-position))))
		              (inherited-tags
		               (or (eq org-agenda-show-inherited-tags 'always)
			           (and (listp org-agenda-show-inherited-tags)
			                (memq 'agenda org-agenda-show-inherited-tags))
			           (and (eq org-agenda-show-inherited-tags t)
			                (or (eq org-agenda-use-tag-inheritance t)
				            (memq 'agenda
				                  org-agenda-use-tag-inheritance)))))
		              (tags (org-get-tags el (not inherited-tags)))
		              (time
		               (cond
		                ;; No time of day designation if it is only
		                ;; a reminder.
		                ((and (/= current deadline) (/= current repeat)) nil)
		                ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
		                 (concat (substring s (match-beginning 1)) " "))
		                (t 'time)))
		              (item
		               (org-agenda-format-item
		                ;; Insert appropriate suffixes before deadlines.
		                ;; Those only apply to today agenda.
		                (pcase-let ((`(,now ,future ,past)
				             org-agenda-deadline-leaders))
		                  (cond
			           ((and today? (< deadline today)) (format past (- diff)))
			           ((and today? (> deadline today)) (format future diff))
			           (t now)))
		                (org-add-props head nil
                                  'effort effort
                                  'effort-minutes effort-minutes)
                                level category tags time))
		              (face (org-agenda-deadline-face
			             (- 1 (/ (float diff) (max wdays 1)))))
		              (upcoming? (and today? (> deadline today)))
		              (warntime (get-text-property (point) 'org-appt-warntime)))
	                 (org-add-props item props
		           'org-marker (org-agenda-new-marker pos)
		           'org-hd-marker (org-agenda-new-marker (line-beginning-position))
		           'warntime warntime
		           'level level
                           'effort effort 'effort-minutes effort-minutes
		           'ts-date deadline
		           'priority
		           ;; Adjust priority to today reminders about deadlines.
		           ;; Overdue deadlines get the highest priority
		           ;; increase, then imminent deadlines and eventually
		           ;; more distant deadlines.
		           (let ((adjust (if today? (- diff) 0)))
		             (+ adjust (org-get-priority item)))
		           'todo-state todo-state
		           'type (if upcoming? "upcoming-deadline" "deadline")
		           'date (if upcoming? date deadline)
		           'face (if done? 'org-agenda-done face)
		           'undone-face face
		           'done-face 'org-agenda-done)
	                 (push item deadline-items)))))))
             :next-re regexp
             :fail-re regexp
             :narrow t)
          (while (re-search-forward regexp nil t)
            (catch :skip
	      (unless (save-match-data (org-at-planning-p)) (throw :skip nil))
	      (org-agenda-skip)
	      (let* ((s (match-string 1))
	             (pos (1- (match-beginning 1)))
	             (todo-state (save-match-data (org-get-todo-state)))
	             (done? (member todo-state org-done-keywords))
                     (sexp? (string-prefix-p "%%" s))
	             ;; DEADLINE is the deadline date for the entry.  It is
	             ;; either the base date or the last repeat, according
	             ;; to `org-agenda-prefer-last-repeat'.
	             (deadline
		      (cond
		       (sexp? (org-agenda--timestamp-to-absolute s current))
		       ((or (eq org-agenda-prefer-last-repeat t)
		            (member todo-state org-agenda-prefer-last-repeat))
		        (org-agenda--timestamp-to-absolute
		         s today 'past (current-buffer) pos))
		       (t (org-agenda--timestamp-to-absolute s))))
	             ;; REPEAT is the future repeat closest from CURRENT,
	             ;; according to `org-agenda-show-future-repeats'. If
	             ;; the latter is nil, or if the time stamp has no
	             ;; repeat part, default to DEADLINE.
	             (repeat
		      (cond
		       (sexp? deadline)
		       ((<= current today) deadline)
		       ((not org-agenda-show-future-repeats) deadline)
		       (t
		        (let ((base (if (eq org-agenda-show-future-repeats 'next)
				        (1+ today)
				      current)))
		          (org-agenda--timestamp-to-absolute
		           s base 'future (current-buffer) pos)))))
	             (diff (- deadline current))
	             (suppress-prewarning
		      (let ((scheduled
		             (and org-agenda-skip-deadline-prewarning-if-scheduled
			          (org-entry-get nil "SCHEDULED"))))
		        (cond
		         ((not scheduled) nil)
		         ;; The current item has a scheduled date, so
		         ;; evaluate its prewarning lead time.
		         ((integerp org-agenda-skip-deadline-prewarning-if-scheduled)
		          ;; Use global prewarning-restart lead time.
		          org-agenda-skip-deadline-prewarning-if-scheduled)
		         ((eq org-agenda-skip-deadline-prewarning-if-scheduled
			      'pre-scheduled)
		          ;; Set pre-warning to no earlier than SCHEDULED.
		          (min (- deadline
			          (org-agenda--timestamp-to-absolute scheduled))
			       org-deadline-warning-days))
		         ;; Set pre-warning to deadline.
		         (t 0))))
	             (wdays (or suppress-prewarning (org-get-wdays s))))
	        (cond
	         ;; Only display deadlines at their base date, at future
	         ;; repeat occurrences or in today agenda.
	         ((= current deadline) nil)
	         ((= current repeat) nil)
	         ((not today?) (throw :skip nil))
	         ;; Upcoming deadline: display within warning period WDAYS.
	         ((> deadline current) (when (> diff wdays) (throw :skip nil)))
	         ;; Overdue deadline: warn about it for
	         ;; `org-deadline-past-days' duration.
	         (t (when (< org-deadline-past-days (- diff)) (throw :skip nil))))
	        ;; Possibly skip done tasks.
	        (when (and done?
		           (or org-agenda-skip-deadline-if-done
			       (/= deadline current)))
	          (throw :skip nil))
	        (save-excursion
	          (re-search-backward "^\\*+[ \t]+" nil t)
	          (goto-char (match-end 0))
	          (let* ((category (org-get-category))
                         (effort (save-match-data (or (get-text-property (point) 'effort)
                                                      (org-entry-get (point) org-effort-property))))
                         (effort-minutes (when effort (save-match-data (org-duration-to-minutes effort))))
		         (level (make-string (org-reduced-level (org-outline-level))
				             ?\s))
		         (head (buffer-substring-no-properties
                                (point) (line-end-position)))
		         (inherited-tags
		          (or (eq org-agenda-show-inherited-tags 'always)
			      (and (listp org-agenda-show-inherited-tags)
			           (memq 'agenda org-agenda-show-inherited-tags))
			      (and (eq org-agenda-show-inherited-tags t)
			           (or (eq org-agenda-use-tag-inheritance t)
				       (memq 'agenda
				             org-agenda-use-tag-inheritance)))))
		         (tags (org-get-tags nil (not inherited-tags)))
		         (time
		          (cond
		           ;; No time of day designation if it is only
		           ;; a reminder.
		           ((and (/= current deadline) (/= current repeat)) nil)
		           ((string-match " \\([012]?[0-9]:[0-9][0-9]\\)" s)
		            (concat (substring s (match-beginning 1)) " "))
		           (t 'time)))
		         (item
		          (org-agenda-format-item
		           ;; Insert appropriate suffixes before deadlines.
		           ;; Those only apply to today agenda.
		           (pcase-let ((`(,now ,future ,past)
				        org-agenda-deadline-leaders))
		             (cond
			      ((and today? (< deadline today)) (format past (- diff)))
			      ((and today? (> deadline today)) (format future diff))
			      (t now)))
		           (org-add-props head nil
                             'effort effort
                             'effort-minutes effort-minutes)
                           level category tags time))
		         (face (org-agenda-deadline-face
			        (- 1 (/ (float diff) (max wdays 1)))))
		         (upcoming? (and today? (> deadline today)))
		         (warntime (get-text-property (point) 'org-appt-warntime)))
	            (org-add-props item props
		      'org-marker (org-agenda-new-marker pos)
		      'org-hd-marker (org-agenda-new-marker (line-beginning-position))
		      'warntime warntime
		      'level level
                      'effort effort 'effort-minutes effort-minutes
		      'ts-date deadline
		      'priority
		      ;; Adjust priority to today reminders about deadlines.
		      ;; Overdue deadlines get the highest priority
		      ;; increase, then imminent deadlines and eventually
		      ;; more distant deadlines.
		      (let ((adjust (if today? (- diff) 0)))
		        (+ adjust (org-get-priority item)))
		      'todo-state todo-state
		      'type (if upcoming? "upcoming-deadline" "deadline")
		      'date (if upcoming? date deadline)
		      'face (if done? 'org-agenda-done face)
		      'undone-face face
		      'done-face 'org-agenda-done)
	            (push item deadline-items)))))))
        (nreverse deadline-items))))

  :bind
  (("s-a" . org-agenda)))

(provide '--org-agenda__agenda_org_organization_schedule@@20250901T121155)
;;; --org-agenda__agenda_org_organization_schedule@@20250901T121155.el ends here
