;;; sd-pg-manager.el  -*- lexical-binding:t -*-

(load-file "./sd-proof-manager.el")
(require 'sd-proof-manager)
(require 'proof-general)
(require 's)

(defconst sd--pg-separator "============================\n")

(defun sd--pg-update-assert-next-command (callback)
  "Adds an hook that calls CALLBACK every time the proof state is updated."
  (add-hook 'proof-state-change-hook (lambda () (run-at-time "0.05 sec" nil callback))))
;; for some reason I *need* to use run-at-time as neither (apply callback) nor (funcall callback) works?

(defun sd--pg-get-goal-buffer-as-string ()
  "Get the Rocq statement of the current goal."
  (with-current-buffer (get-buffer "*goals*") (buffer-string)))

(defun sd--pg-get-current-goal ()
  "Gets the current goal from the *goals* buffer and returns it"
  (let* ((goals (sd--pg-get-goal-buffer-as-string))
		 (split (split-string goals "\ngoal")))
	(cadr (split-string (car split) sd--pg-separator))))

(defun sd--pg-get-current-hypotheses (pred)
  "Gets the current hypotheses satisfying PRED from the *goals* buffer and returns it"
  (let* ((goals (sd--pg-get-goal-buffer-as-string))
		 (candidates (car (split-string goals sd--pg-separator)))
		 (split-candidates (s-slice-at "\n  [^:]+:" candidates)))
	(cl-remove-if-not pred split-candidates)))

(defvar sd--pg-manager
  (make-proof-manager
   :setup-hook #'sd--pg-update-assert-next-command
   :current-goal #'sd--pg-get-current-goal
   :current-hypotheses #'sd--pg-get-current-hypotheses)
  "The `sd-proof-manager' for proof general.")

(provide 'sd-pg-manager)
