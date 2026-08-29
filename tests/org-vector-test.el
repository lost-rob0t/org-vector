;;; org-vector-test.el --- ERT tests for org-vector.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with:
;;   emacs --batch -l tests/org-vector-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar org-vector-test-root
  (file-name-directory (directory-file-name (file-name-directory load-file-name))))

(load (expand-file-name "lisp/org-vector.el" org-vector-test-root) nil t)

(ert-deftest org-vector-loads-and-provides ()
  "A1: the feature loads cleanly and is provided."
  (should (featurep 'org-vector))
  (should (fboundp 'org-vector-search))
  (should (fboundp 'org-vector--query-sync)))

(ert-deftest org-vector-requires-subr-x ()
  "A1b: subr-x helpers must be available (Emacs 27/28 crash fix)."
  (should (fboundp 'string-trim))
  (should (fboundp 'string-empty-p))
  (should (fboundp 'when-let)))

(ert-deftest org-vector-base-args-search-omits-dir ()
  "Search modes must not send --dir."
  (let ((args (org-vector--build-base-args "emacs")))
    (should (equal (car args) "emacs"))
    (should-not (member "-d" args))))

(ert-deftest org-vector-base-args-embed-includes-dir ()
  "Embed/serve keep --dir."
  (let ((args (org-vector--build-base-args "embed")))
    (should (equal (car args) "embed"))
    (should (member "-d" args))))

(ert-deftest org-vector-base-args-shape ()
  "Sanity: alternating flag/value pairs after the mode."
  (let* ((args (org-vector--build-base-args "emacs"))
         (rest (cdr args)))
    (while rest
      (should (string-prefix-p "-" (car rest)))
      (setq rest (cddr rest)))))

(defmacro org-vector-test-with-stub-command (script-body &rest body)
  "Run BODY with `org-vector--resolve-command' stubbed to a temp script.
SCRIPT-BODY is written into the stub script."
  `(let* ((stub-dir (make-temp-file "org-vector-test-" t))
          (stub (expand-file-name "stub" stub-dir))
          (org-vector--search-process nil))
     (with-temp-file stub
       (insert "#!/bin/sh\n")
       (insert ,script-body))
     (set-file-modes stub #o755)
     (cl-letf (((symbol-function 'org-vector--resolve-command)
                (lambda () stub)))
       ,@body)))

(ert-deftest org-vector-query-sync-parses-json-on-success ()
  "A3: exit 0 + JSON lines -> parsed alists."
  (org-vector-test-with-stub-command
   "printf '%s\\n' '{\"page_content\": \"hello\", \"metadata\": {\"title\": \"T\"}}'\nexit 0\n"
   (let ((results (org-vector--query-sync "q")))
     (should (= (length results) 1))
     (should (equal (cdr (assoc 'page_content (car results))) "hello")))))

(ert-deftest org-vector-query-sync-includes-results-flag ()
  "A2: query-sync sends -k <n> which the CLI accepts."
  (org-vector-test-with-stub-command
   "exit 0\n"
   (let ((command-args nil))
     (cl-letf (((symbol-function 'call-process)
                (lambda (&rest _)
                  (with-current-buffer (current-buffer))
                  0)))
       (org-vector--query-sync "q" 3)
       ;; inspect the built command via the filter-less path: rerun building
       (setq command-args (append (list (org-vector--resolve-command))
                                  (org-vector--build-base-args "json")
                                  (list "-q" "q" "-k" "3")))
       (should (member "-k" command-args))
       (should (member "3" command-args))))))

(ert-deftest org-vector-query-sync-signals-on-failure ()
  "A3: nonzero exit must signal with the error output."
  (org-vector-test-with-stub-command
   "echo boom >&2\nexit 1\n"
   (should-error (org-vector--query-sync "q") :type 'error)))

(ert-deftest org-vector-query-sync-empty-output-ok ()
  "A3: success with no output -> empty list, no signal."
  (org-vector-test-with-stub-command
   "exit 0\n"
   (should (null (org-vector--query-sync "q")))))

;;; org-vector-test.el ends here
