;;; sd-mode.el ---- interface between Rocq modes and the string diagrams utility  -*- lexical-binding:t -*-

;; Author: Johann Rosain <johann.rosain@ens-lyon.org>
;; Version: 0.1
;; Package-Requires: ((request "0.3.2") (json "1.5"))
;; Keywords: coq, rocq, string diagrams
;; URL: https://github.com/damien-pous/string-diagrams

(require 'request)
(require 'json)

; TODO: only in dev mode
(load-file "./sd-pg-manager.el")
(require 'sd-pg-manager)

;;; Code:

;; Setup variables:

(defvar sd--prefix "SD: ")

(defvar sd--log-buffer (get-buffer-create "*sd*"))

(defvar sd--raw-separator
  "------")

(defvar sd--address "http://127.0.0.1")

(defvar sd--port nil)

(defcustom sd-default-port 8080
  "Default port for SD WebSocket server."
  :type 'integer)

(defcustom sd-prog-name "/home/johann/dev/string-diagrams/_build/default/bin/sd.exe" ; nil
  "Path to the string diagrams executable."
  :type 'string)

(defcustom sd-morphisms-notation "~>"
  "Notation for a morphism between two objects."
  :type 'string)

(defcustom sd-equality-notation "≡"
  "Notation for equality between morphisms."
  :type 'string)

(defun sd--format (msg)
  (concat sd--prefix msg))

(defun sd--get-address ()
  (concat sd--address ":" (number-to-string sd--port) "/"))

;; Communication with the server:

(defun sd--get-from-response-data (response-data key)
  (let ((table (json-parse-string response-data :object-type 'alist)))
	(alist-get key table)))

(defun sd--get-response-status (response-data)
  (sd--get-from-response-data response-data 'status))

(defun sd--get-failure-reason (response-data)
  (sd--get-from-response-data response-data 'reason))

(defun sd--send-message-raw (goal hypotheses)
  "Sends raw message to the server by joining each element of HYPOTHESES using `\n',
adding the separator and appending the GOAL."
  (when (not (string-empty-p goal))
	(let ((data (concat (mapconcat 'identity hypotheses "\n") "\n" sd--raw-separator "\n" goal)))
	  (request
		(concat (sd--get-address) "raw")
		:type "POST"
		:data data
		:complete
		(cl-function
		 (lambda (&key response &allow-other-keys)
		   (let* ((response-data (request-response-data response))
				  (status (sd--get-response-status response-data)))
			 (when (not status)
			   (message (sd--format "Failed to update view. Reason: %s")
						(sd--get-failure-reason response-data))))))))))

(defun sd--launch-for-current-buffer (port)
  "Launches an SD process on PORT for the current buffer."
  ; currently, the PORT argument is ignored by SD
  ; we use setq-local to be able to have multiple instances of SD for multiple buffers
  (start-process "sd" sd--log-buffer sd-prog-name))

;;;###autoload
(defun sd-launch-graphical-display (&optional port)
  "Starts SD on PORT (default `sd-default-port')"
  (interactive (list (read-number "Port: " sd-default-port)))
  (sd--launch-for-current-buffer port)
  (setq-local sd--port port))

(defvar-keymap sd-pg-mode-map
  "C-c C-d" #'sd-launch-graphical-display)

(defun sd--assert-next-interactive (pm)
  "Sends the current goal to the graphical display."
  (if sd--port
	  (sd--send-message-raw
	   (funcall (proof-manager-current-goal pm))
	   (funcall (proof-manager-current-hypotheses pm) (lambda (s) (or (string-search sd-morphisms-notation s) (string-search sd-equality-notation s)))))
	(warn (sd--format "no socket opened to send messages to the graphical view. Try restarting the graphical display with `sd-launch-graphical-display'"))))

(defun sd--setup-keymap (keymap pm)
  (funcall (proof-manager-setup-hook pm) (lambda () (sd--assert-next-interactive pm)) keymap))

(define-minor-mode sd-pg-mode
  "Interface with the string diagram utility for proof general"
  :init-value nil
  :global nil
  (sd--setup-keymap sd-pg-mode-map sd--pg-manager)
  (setq-local sd--port nil))

(provide 'sd-pg-mode)
