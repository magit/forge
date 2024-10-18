;;; forge-commands.el --- Commands  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.forge@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.forge@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'forge)

;;; Options

(defcustom forge-add-pullreq-refspec t
  "Whether the pull-request refspec is added when setting up a repository.

This controls whether running `forge-pull' for the first time in
a repository also adds a refspec that fetches all pull-requests.
In repositories with huge numbers of pull-requests you might want
to not do so, in which case you should set this option to `ask'.

You can also set this to nil and later add the refspec explicitly
for a repository using the command `forge-add-pullreq-refspec'."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type '(choice (const :tag "Always add refspec" t)
                 (const :tag "Ask every time" ask)
                 (const :tag "Never add refspec" nil)))

(defcustom forge-checkout-worktree-read-directory-function
  'forge-checkout-worktree-default-read-directory-function
  "Function used by `forge-checkout-worktree' to read worktree directory.
Takes the pull-request as only argument and must return a directory."
  :package-version '(forge . "0.4.0")
  :group 'forge
  :type 'function)

;;; Dispatch

;;;###autoload(autoload 'forge-dispatch "forge-commands" nil t)
(transient-define-prefix forge-dispatch ()
  "Dispatch a forge command."
  :transient-non-suffix #'transient--do-call
  :refresh-suffixes t
  :environment #'forge--menu-environment
  :column-widths forge--topic-menus-column-widths
  [forge--topic-menus-group
   ["Fetch"
    ("f f" "all topics"     forge-pull)
    ("f t" "one topic"      forge-pull-topic)
    ("f n" "notifications"  forge-pull-notifications)]
   ["Create"
    :if forge--get-repository:tracked?
    ("c i" "issue"          forge-create-issue)
    ("c p" "pull-request"   forge-create-pullreq)
    ("c u" "pr from issue"  forge-create-pullreq-from-issue)
    ("c f" "fork or remote" forge-fork)]
   [:description (lambda ()
                   (cond
                    ((forge--get-repository:tracked?) "Actions")
                    ((or (magit-gitdir) (forge-repository-at-point))
                     "Forge does not yet track this repository")
                    ("Not inside a Git repository")))
    ("/a" forge-add-repository
     :description (lambda () (let ((repo (forge-get-repository :stub?)))
                          (if (or (not repo)
                                  (eq (oref repo condition) :tracked))
                            "track some repo"
                          "track this repository"))))
    ("/M" "merge with api" forge-merge
     :if forge--get-repository:tracked? :level 7)]]
  [forge--lists-group
   ["Visit"
    :inapt-if-not forge--get-repository:tracked?
    ("v t" "topic"          forge-visit-topic)
    ("v i" "issue"          forge-visit-issue)
    ("v p" "pull-request"   forge-visit-pullreq)]
   ["Browse"
    ("b t" "topic"          forge-browse-topic
     :inapt-if-not forge--get-repository:tracked?)
    ("b i" "issue"          forge-browse-issue
     :inapt-if-not forge--get-repository:tracked?)
    ("b p" "pull-request"   forge-browse-pullreq
     :inapt-if-not forge--get-repository:tracked?)
    ("b r" "remote"         forge-browse-remote)
    ("b I" "issues"         forge-browse-issues)
    ("b P" "pull-requests"  forge-browse-pullreqs)
    ""]
   ["Display"
    ("-S" forge-toggle-display-in-status-buffer
     :inapt-if-not forge--buffer-with-topics-sections-p)
    ("-H" forge-toggle-topic-legend)]]
  [forge--topic-legend-group])

(transient-augment-suffix forge-dispatch
  :transient #'transient--do-replace
  :inapt-if (lambda () (eq (oref transient--prefix command) 'forge-dispatch))
  :inapt-face 'forge-suffix-active)

;;;###autoload(autoload 'forge-configure "forge-commands" nil t)
(transient-define-prefix forge-configure ()
  "Configure current repository and global settings."
  :transient-non-suffix #'transient--do-call
  :refresh-suffixes t
  :environment #'forge--menu-environment
  :column-widths forge--topic-menus-column-widths
  [forge--topic-menus-group
   ["Configure"
    ("R  " forge-add-pullreq-refspec)
    ("s r" forge-forge.remote)
    ("s l" forge-forge.graphqlItemLimit)]])

(transient-augment-suffix forge-configure
  :transient #'transient--do-replace
  :inapt-if (lambda () (eq (oref transient--prefix command) 'forge-configure))
  :inapt-face 'forge-suffix-active)

;;; Pull

;;;###autoload(autoload 'forge-pull "forge-commands" nil t)
(transient-define-suffix forge-pull ()
  "Pull forge topics for the current repository if it is already tracked.
If the current repository is still untracked locally, or the current
repository cannot be determined, instead invoke `forge-add-repository'."
  :description (lambda ()
                 (if (forge-get-repository :tracked?)
                     "forge topics"
                   "new forge repository"))
  (declare (interactive-only nil))
  (interactive)
  (if-let ((repo (forge-get-repository :tracked?)))
      (forge--pull repo)
    (transient-setup 'forge-add-repository nil nil
                     :scope (forge-add-repository--scope))))

(defun forge-read-date (prompt)
  (require (quote org) nil)
  (if (fboundp 'org-read-date)
      (org-read-date nil nil nil prompt)
    (cl-block nil
      (while t
        (let ((str (read-from-minibuffer prompt)))
          (cond ((string-equal str "")
                 (cl-return nil))
                ((string-match-p
                  "\\`[0-9]\\{4\\}[-/][0-9]\\{2\\}[-/][0-9]\\{2\\}\\'" str)
                 (cl-return str))))
        (message "Please enter a date in the format YYYY-MM-DD.")
        (sit-for 1)))))

(cl-defmethod forge--pull ((repo forge-noapi-repository) &rest _)
  (forge--msg repo t t "Pulling from REPO is not supported"))

(cl-defmethod forge--pull ((repo forge-unusedapi-repository) &rest _)
  (magit-git-fetch (oref repo remote) (magit-fetch-arguments)))

(defun forge--maybe-git-fetch (repo &optional buffer)
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (if (and (derived-mode-p 'magit-mode)
                 (forge-repository-equal (forge-get-repository :stub?) repo)
                 (magit-toplevel))
            (magit-git-fetch (oref repo remote) (magit-fetch-arguments))
          (magit-refresh-buffer)))
    (when-let ((worktree (forge-get-worktree repo)))
      (let ((default-directory worktree)
            (magit-inhibit-refresh t))
        (magit-git-fetch (oref repo remote) (magit-fetch-arguments))))))

;;;###autoload(autoload 'forge-pull-notifications "forge-commands" nil t)
(transient-define-suffix forge-pull-notifications ()
  "Fetch notifications for all repositories from the current forge."
  :description "forge notifications"
  (interactive)
  (if-let ((repo (forge-get-repository :stub?)))
      (let ((class (eieio-object-class repo)))
        (if (eq class 'forge-github-repository)
            (forge--pull-notifications class (oref repo githost))
          (user-error "Fetching notifications not supported for forge %S"
                      (oref repo forge))))
    (forge--pull-notifications 'forge-github-repository "github.com")))

;;;###autoload(autoload 'forge-pull-topic "forge-commands" nil t)
(transient-define-suffix forge-pull-topic (number)
  "Read a topic TYPE and NUMBER pull data about it from its forge."
  :inapt-if-not (lambda () (and (forge--get-repository:tracked?)
                           (forge--get-github-repository)))
  (interactive
   (list (read-number "Pull topic: "
                      (and-let* ((topic (forge-current-topic)))
                        (oref topic number)))))
  (forge--pull-topic (forge-get-repository :tracked) number))

;;;###autoload(autoload 'forge-pull-this-topic "forge-commands" nil t)
(transient-define-suffix forge-pull-this-topic ()
  "Pull data about the topic at point from its forge."
  :inapt-if-not #'forge--get-github-repository
  :description "fetch"
  (interactive)
  (let ((topic (forge-current-topic t)))
    (forge--pull-topic (forge-get-repository topic) topic)))

(cl-defmethod forge--pull-topic ((repo forge-repository) _topic)
  (error "Fetching an individual topic not implemented for %s"
         (eieio-object-class repo)))

;;; Browse

;;;###autoload
(defun forge-browse-issues ()
  "Visit the current repository's issues using a browser."
  (interactive)
  (browse-url (forge--format (forge-get-repository :stub)
                             'issues-url-format)))

;;;###autoload
(defun forge-browse-pullreqs ()
  "Visit the current repository's pull-requests using a browser."
  (interactive)
  (browse-url (forge--format (forge-get-repository :stub)
                             'pullreqs-url-format)))

;;;###autoload
(defun forge-browse-topic (topic)
  "Read a TOPIC and visit it using a browser.
By default only offer open topics but with a prefix argument
also offer closed topics."
  (interactive (list (forge-read-topic "Browse topic")))
  (forge--browse-topic topic))

;;;###autoload
(defun forge-browse-issue (issue)
  "Read an ISSUE and visit it using a browser.
By default only offer open issues but with a prefix argument
also offer closed issues."
  (interactive (list (forge-read-issue "Browse issue")))
  (forge--browse-topic issue))

;;;###autoload
(defun forge-browse-pullreq (pull-request)
  "Read a PULL-REQUEST and visit it using a browser.
By default only offer open pull-requests but with a prefix
argument also offer closed pull-requests."
  (interactive (list (forge-read-pullreq "Browse pull-request")))
  (forge--browse-topic pull-request))

(defun forge--browse-topic (topic)
  (let ((obj (forge-get-topic topic)))
    (browse-url (forge-get-url obj))
    (forge-topic-mark-read obj)))

;;;###autoload
(defun forge-browse-commit (commit)
  "Read a COMMIT and visit it using a browser."
  (interactive
   (list (or (magit-completing-read "Browse commit"
                                    (magit-list-branch-names)
                                    nil nil nil 'magit-revision-history
                                    (magit-branch-or-commit-at-point))
             (user-error "Nothing selected"))))
  (browse-url (forge-get-url :commit commit)))

;;;###autoload
(defun forge-browse-branch (branch)
  "Read a BRANCH and visit it using a browser."
  (interactive (list (magit-read-branch "Browse branch")))
  (browse-url (forge-get-url :branch branch)))

;;;###autoload
(defun forge-browse-remote (remote)
  "Read a REMOTE and visit it using a browser."
  (interactive (list (magit-read-remote "Browse remote" nil t)))
  (browse-url (forge-get-url :remote remote)))

;;;###autoload
(defun forge-browse-repository (repository)
  "Read a REPOSITORY and visit it using a browser."
  (interactive (list (forge-read-repository "Browse repository")))
  (browse-url (forge-get-url repository)))

;;;###autoload(autoload 'forge-browse-this-topic "forge-commands" nil t)
(transient-define-suffix forge-browse-this-topic ()
  "Visit the topic at point using a browser."
  :description "browse"
  (interactive)
  (forge-browse-topic (forge-current-topic t)))

;;;###autoload
(defun forge-browse-this-repository ()
  "Visit the repository at point using a browser."
  (interactive)
  (forge-browse-repository (forge-repository-at-point t)))

;;;###autoload
(defun forge-copy-url-at-point-as-kill ()
  "Copy the url of the thing at point."
  (interactive)
  (if-let ((target (forge--browse-target)))
      (let ((url (if (stringp target) target (forge-get-url target))))
        (kill-new url)
        (message "Copied \"%s\"" url))
    (user-error "Nothing at point with a URL")))

;;;###autoload
(defun forge-browse ()
  "Visit the thing at point using a browser."
  (interactive)
  (if-let ((target (forge--browse-target)))
      (if (stringp target)
          (browse-url target)
        (browse-url (forge-get-url target))
        (when (cl-typep target 'forge-topic)
          (forge-topic-mark-read target)))
    (user-error "Nothing to browse here")))

(defun forge--browse-target ()
  (or (and-let* ((branch (magit--painted-branch-at-point)))
        (forge-get-url :branch branch))
      (and-let* ((commit (magit-commit-at-point)))
        (forge-get-url :commit commit))
      (and-let* ((branch (magit-branch-at-point)))
        (forge-get-url :branch branch))
      (and-let* ((remote (magit-remote-at-point)))
        (forge-get-url :remote remote))
      (forge-post-at-point)
      (forge-current-topic)
      (and magit-buffer-revision
           (forge-get-url :commit magit-buffer-revision))
      (forge-get-repository :stub?)))

;;;; Urls

(cl-defgeneric forge-get-url (obj)
  "Return the URL for a forge object.")

(cl-defmethod forge-get-url ((issue forge-issue))
  (forge--format issue 'issue-url-format))

(cl-defmethod forge-get-url ((pullreq forge-pullreq))
  (forge--format pullreq 'pullreq-url-format))

(cl-defmethod forge-get-url ((repo forge-repository))
  (forge--format repo 'remote-url-format))

(cl-defmethod forge-get-url ((_(eql :commit)) commit)
  (let ((repo (forge-get-repository :stub)))
    (unless (magit-list-containing-branches
             commit "-r" (concat (oref repo remote) "/*"))
      (if-let* ((branch (car (magit-list-containing-branches commit "-r")))
                (remote (cdr (magit-split-branch-name branch))))
          (setq repo (forge-get-repository :stub remote))
        (message "%s does not appear to be available on any remote.  %s"
                 commit "You might have to push it first.")))
    (forge--format repo 'commit-url-format
                   `((?r . ,(magit-commit-p commit))))))

(cl-defmethod forge-get-url ((_(eql :branch)) branch)
  (let (remote)
    (if (magit-remote-branch-p branch)
        (let ((cons (magit-split-branch-name branch)))
          (setq remote (car cons))
          (setq branch (cdr cons)))
      (unless (setq remote (or (magit-get-push-remote branch)
                               (magit-get-upstream-remote branch)))
        (user-error "Cannot determine remote for %s" branch)))
    (forge--format (forge-get-repository :stub remote)
                   'branch-url-format
                   `((?r . ,branch)))))

(cl-defmethod forge-get-url ((_(eql :remote)) remote)
  (forge--format (forge-get-repository :stub remote) 'remote-url-format))

(cl-defmethod forge-get-url ((post forge-post))
  (forge--format post (let ((topic (forge-get-parent post)))
                        (cond ((forge--childp topic 'forge-issue)
                               'issue-post-url-format)
                              ((forge--childp topic 'forge-pullreq)
                               'pullreq-post-url-format)))))

(cl-defmethod forge-get-url ((notify forge-notification))
  (oref notify url))

;;; Visit

;;;###autoload
(defun forge-visit-topic (topic)
  "Read a TOPIC and visit it.
By default only offer active topics for completion.  With a prefix
argument offer all topics.  While completion is in progress, \
\\<forge-read-topic-minibuffer-map>\\[forge-read-topic-lift-limit] lifts
the limitation to active topics."
  (interactive (list (forge-read-topic "View topic")))
  (forge-topic-setup-buffer (forge-get-topic topic)))

;;;###autoload
(defun forge-visit-issue (issue)
  "Read an ISSUE and visit it.
By default only offer active issues for completion.  With a prefix
argument offer all topics.  While completion is in progress, \
\\<forge-read-topic-minibuffer-map>\\[forge-read-topic-lift-limit] lifts
the limitation to active issues."
  (interactive (list (forge-read-issue "View issue")))
  (forge-topic-setup-buffer (forge-get-issue issue)))

;;;###autoload
(defun forge-visit-pullreq (pull-request)
  "Read a PULL-REQUEST and visit it.
By default only offer active pull-requests for completion.  With a
prefix argument offer all topics.  While completion is in progress,
\\<forge-read-topic-minibuffer-map>\\[forge-read-topic-lift-limit] \
lifts the limitation to active pull-requests."
  (interactive (list (forge-read-pullreq "View pull-request")))
  (forge-topic-setup-buffer (forge-get-pullreq pull-request)))

;;;###autoload
(defun forge-visit-this-topic (&optional menu)
  "Visit the topic at point.
With prefix argument MENU, also show the topic menu."
  (interactive (list current-prefix-arg))
  (forge-topic-setup-buffer (forge-topic-at-point))
  (cond
   ((eq transient-current-command 'forge-topic-menu)
    (setq forge--quit-keep-topic-menu t))
   ((or menu
        (memq transient-current-command
              '(forge-topics-menu forge-notifications-menu)))
    (transient-setup 'forge-topic-menu))))

;;;###autoload
(defun forge-visit-this-repository ()
  "Visit the repository at point."
  (interactive)
  (let* ((repo (forge-repository-at-point))
         (worktree (forge-get-worktree repo)))
    (cond
     ((and (eq transient-current-command 'forge-repositories-menu)
           (forge-get-repository repo nil :tracked?))
      (if-let ((buffer (get-buffer (forge-topics-buffer-name repo))))
          (progn (switch-to-buffer buffer)
                 (transient-setup 'forge-topics-menu))
        (forge-list-topics repo)))
     (worktree
      (magit-status-setup-buffer worktree))
     ((forge-get-repository repo nil :tracked?)
      (forge-list-topics repo))
     ((user-error "Not tracked and location of clone is unknown")))))

;;; Create

(defun forge-create-issue ()
  "Create a new issue for the current repository."
  (interactive)
  (let* ((repo (forge-get-repository :tracked))
         (buf (forge--prepare-post-buffer
               "new-issue"
               (forge--format repo "Create new issue on %p"))))
    (when buf
      (with-current-buffer buf
        (setq forge--buffer-post-object repo)
        (setq forge--submit-post-function #'forge--submit-create-issue))
      (forge--display-post-buffer buf))))

(defun forge-create-pullreq (source target)
  "Create a new pull-request for the current repository."
  (interactive (forge-create-pullreq--read-args))
  (let* ((repo (forge-get-repository :tracked))
         (buf (forge--prepare-post-buffer
               "new-pullreq"
               (forge--format repo "Create new pull-request on %p")
               source target)))
    (with-current-buffer buf
      (setq forge--buffer-base-branch target)
      (setq forge--buffer-head-branch source)
      (setq forge--buffer-post-object repo)
      (setq forge--submit-post-function #'forge--submit-create-pullreq)
      (run-hooks 'forge-create-pullreq-hook))
    (forge--display-post-buffer buf)))

(transient-define-suffix forge-create-pullreq-from-issue (issue source target)
  "Convert an existing ISSUE into a pull-request."
  :description "convert to pull-request"
  :if (lambda ()
        (and (forge--get-github-repository)
             (let ((issue (forge-current-issue)))
               (and issue (eq (oref issue state) 'open)
                    issue))))
  (interactive (cons (forge-read-open-issue "Convert issue")
                     (forge-create-pullreq--read-args)))
  (setq issue (forge-get-issue issue))
  (forge--create-pullreq-from-issue (forge-get-repository issue)
                                    issue source target))

(defun forge-create-pullreq--read-args ()
  (let* ((source  (magit-completing-read
                   "Source branch"
                   (magit-list-remote-branch-names)
                   nil t nil 'magit-revision-history
                   (or (and-let* ((d (magit-branch-at-point)))
                         (if (magit-remote-branch-p d)
                             d
                           (magit-get-push-branch d t)))
                       (and-let* ((d (magit-get-current-branch)))
                         (if (magit-remote-branch-p d)
                             d
                           (magit-get-push-branch d t))))))
         (repo    (forge-get-repository :tracked))
         (remote  (oref repo remote))
         (targets (delete source (magit-list-remote-branch-names remote)))
         (target  (magit-completing-read
                   "Target branch" targets nil t nil 'magit-revision-history
                   (let* ((d (cdr (magit-split-branch-name source)))
                          (d (and (magit-branch-p d) d))
                          (d (and d (magit-get-upstream-branch d)))
                          (d (and d (if (magit-remote-branch-p d)
                                        d
                                      (magit-get-upstream-branch d))))
                          (d (or d (concat remote "/"
                                           (or (oref repo default-branch)
                                               "master")))))
                     (car (member d targets))))))
    (list source target)))

(defun forge-create-post (&optional quote)
  "Create a new post on an existing topic.
If the region is active, then quote that part of the post.
Otherwise and with a prefix argument quote the post that
point is currently on."
  (interactive (list current-prefix-arg))
  (unless (derived-mode-p 'forge-topic-mode)
    (user-error "This command is only available from topic buffers"))
  (let* ((topic forge-buffer-topic)
         (buf (forge--prepare-post-buffer
               (forge--format topic "%i;new-comment")
               (forge--format topic "New comment on #%i of %p")))
         (quote (cond
                 ((not (magit-section-match 'post)) nil)
                 ((use-region-p)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end)))
                 (quote
                  (let ((section (magit-current-section)))
                    (string-trim-right
                     (buffer-substring-no-properties (oref section content)
                                                     (oref section end))))))))
    (with-current-buffer buf
      (setq forge--buffer-post-object topic)
      (setq forge--submit-post-function #'forge--submit-create-post)
      (when quote
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n"))
        (insert (replace-regexp-in-string "^" "> " quote) "\n\n")))
    (forge--display-post-buffer buf)))

;;; Edit

(defun forge-edit-post ()
  "Edit the current post."
  (interactive)
  (let* ((post (forge-post-at-point t))
         (buf (cl-typecase post
                (forge-topic
                 (forge--prepare-post-buffer
                  (forge--format post "%i")
                  (forge--format post "Edit #%i of %p")))
                (forge-post
                 (forge--prepare-post-buffer
                  (forge--format post "%i;%I")
                  (forge--format post "Edit comment on #%i of %p"))))))
    (with-current-buffer buf
      (setq forge--buffer-post-object post)
      (setq forge--submit-post-function #'forge--submit-edit-post)
      (erase-buffer)
      (when (cl-typep post 'forge-topic)
        (insert "# " (oref post title) "\n\n"))
      (insert (oref post body)))
    (forge--display-post-buffer buf)))

(transient-define-suffix forge-edit-topic-note ()
  "Edit your private note about the current topic."
  :transient #'transient--do-quit-all
  :description
  (lambda ()
    (if-let ((topic (forge-current-topic)))
        (concat "note "
                (if-let ((note (oref topic note)))
                    (propertize (substring note 0 (string-match-p "$" note))
                                'face 'font-lock-string-face)
                  (propertize "none" 'face 'magit-dimmed)))
      "note"))
  (interactive)
  (if-let* ((topic (forge-current-topic t))
            (repo (forge-get-repository topic))
            (default-directory (forge-get-worktree repo))
            (buf (forge--prepare-post-buffer
                  (forge--format topic "%i;note")
                  (forge--format topic "New note on #%i of %p"))))
      (progn
        (with-current-buffer buf
          (setq forge--buffer-post-object topic)
          (setq forge--submit-post-function #'forge--save-note)
          (erase-buffer)
          (when-let ((note (oref topic note)))
            (save-excursion (insert note ?\n))))
        (forge--display-post-buffer buf))
    (message "Cannot determine topic or worktree")))

;;; Delete

(transient-define-suffix forge-delete-comment ()
  "Delete the comment at point."
  :description "delete comment"
  :inapt-if-not #'forge-comment-at-point
  (interactive)
  (let ((comment (forge-comment-at-point t)))
    (when (yes-or-no-p "Really delete the current comment? ")
      (forge--delete-comment (forge-get-repository :tracked) comment))))

;;; Branch

;;;###autoload
(defun forge-branch-pullreq (pullreq)
  "Create and configure a new branch from a pull-request.
Please see the manual for more information."
  (interactive (list (forge-read-pullreq "Branch pull request")))
  (let ((pullreq (forge-get-pullreq pullreq)))
    (if-let ((branch (forge--pullreq-branch-active pullreq)))
        (progn (message "Branch %S already exists and is configured" branch)
               branch)
      (forge--branch-pullreq (forge-get-repository pullreq) pullreq)
      (forge-refresh-buffer))))

(cl-defmethod forge--branch-pullreq ((pullreq forge-pullreq))
  (forge--branch-pullreq (forge-get-repository pullreq) pullreq))

(cl-defmethod forge--branch-pullreq ((_repo forge-unusedapi-repository) pullreq)
  ;; We don't know enough to do a good job.
  (let* ((number (oref pullreq number))
         (branch (format "pr-%s" number)))
    (when (magit-branch-p branch)
      (user-error "Branch `%s' already exists" branch))
    (magit-git "branch" branch (forge--pullreq-ref pullreq))
    ;; More often than not this is the correct target branch.
    (magit-call-git "branch" branch "--set-upstream-to=master")
    (magit-set (number-to-string number) "branch" branch "pullRequest")
    branch))

(cl-defmethod forge--branch-pullreq ((repo forge-repository) pullreq)
  (let* ((number (oref pullreq number))
         (branch-n (format "pr-%s" number))
         (branch (or (forge--pullreq-branch-internal pullreq) branch-n))
         (pullreq-ref (format "refs/pullreqs/%s" number)))
    (cond ((and-let* ((pr-branch (oref pullreq head-ref)))
             (string-search ":" pr-branch))
           ;; Such a branch name would be invalid.  If we encounter
           ;; it anyway, then that means that the source branch and
           ;; the merge-request ref are missing.  Luckily Gitlab no
           ;; longer does this, but we nevertheless have to deal
           ;; with merge-requests that have been lost in time.
           (error "Cannot check out this merge-request because %s"
                  "on old Gitlab version discarded the source branch"))
          ((not (eq (oref pullreq state) 'open))
           (magit-git "branch" "--force" branch pullreq-ref))
          (t
           (let ((upstream  (oref repo remote))
                 (pr-remote (oref pullreq head-user))
                 (pr-branch (oref pullreq head-ref)))
             (cond ((not (oref pullreq cross-repo-p))
                    (let ((tracking (concat upstream "/" pr-branch)))
                      (unless (magit-branch-p tracking)
                        (magit-call-git "fetch" upstream))
                      (forge--setup-pullreq-branch branch tracking)
                      (magit-branch-maybe-adjust-upstream branch tracking)
                      (magit-set upstream "branch" branch "pushRemote")
                      (magit-set upstream "branch" branch "pullRequestRemote")))
                   ((not pr-branch)
                    ;; The pullreq branch (on Github) has been deleted.
                    (setq pr-remote nil)
                    (setq branch branch-n)
                    (forge--setup-pullreq-branch branch pullreq-ref)
                    (magit-set upstream "branch" branch "pushRemote"))
                   (t
                    ;; For prs within the upstream we are more permissive,
                    ;; but any request to merge a branch with a well known
                    ;; name from fork, is highly suspicious and likely the
                    ;; result of a contributor not bothering to name their
                    ;; feature branch.
                    (when (and (member branch magit-main-branch-names)
                               (magit-branch-p branch))
                      (setq branch branch-n))
                    (forge--setup-pullreq-remote pullreq)
                    (forge--setup-pullreq-branch
                     branch (concat pr-remote "/" pr-branch))
                    (if (and (oref pullreq editable-p)
                             (equal branch pr-branch))
                        (magit-set pr-remote "branch" branch "pushRemote")
                      (magit-set upstream "branch" branch "pushRemote"))))
             (when pr-remote
               (magit-set pr-remote "branch" branch "pullRequestRemote"))
             (magit-set "true" "branch" branch "rebase")
             (magit-git "branch" branch
                        (let ((base-ref (oref pullreq base-ref)))
                          (concat "--set-upstream-to="
                                  (if (or magit-branch-prefer-remote-upstream
                                          (not (magit-branch-p base-ref)))
                                      (concat upstream "/" base-ref)
                                    base-ref)))))))
    (magit-set (number-to-string number) "branch" branch "pullRequest")
    (magit-set (oref pullreq title) "branch" branch "description")
    branch))

(defun forge--setup-pullreq-branch (branch tracking)
  (if (magit-branch-p branch)
      (unless (magit-rev-equal branch tracking)
        (message "Existing branch %s diverged from %s" branch tracking))
    (magit-git "branch" branch tracking)))

(defun forge--setup-pullreq-remote (pullreq)
  (let* ((pr-remote (oref pullreq head-user))
         (pr-branch (oref pullreq head-ref))
         (repo (forge-get-repository pullreq))
         (host (oref repo githost))
         (user (oref pullreq head-user))
         (fork (oref pullreq head-repo)))
    (if (magit-remote-p pr-remote)
        (let ((url (magit-git-string "remote" "get-url" pr-remote))
              (fetch (magit-get-all "remote" pr-remote "fetch")))
          (unless (equal (forge--split-forge-url url)
                         (list host user (substring fork (1+ (length user)))))
            (user-error "Remote `%s' already exists but does not point to %s"
                        pr-remote url))
          (unless (or (member (format "+refs/heads/*:refs/remotes/%s/*"
                                      pr-remote)
                              fetch)
                      (member (format "+refs/heads/%s:refs/remotes/%s/%s"
                                      pr-branch pr-remote pr-branch)
                              fetch))
            (magit-git "remote" "set-branches" "--add" pr-remote pr-branch)
            (magit-git "fetch" pr-remote)))
      (let ((url (magit-git-string "remote" "get-url" (oref repo remote))))
        (magit-git
         "remote" "add" "-f" "--no-tags"
         "-t" pr-branch pr-remote
         (cond ((or (string-prefix-p "git@" url)
                    (string-prefix-p "ssh://git@" url))
                (format "git@%s:%s.git" host fork))
               ((string-prefix-p "https://" url)
                (format "https://%s/%s.git" host fork))
               ((string-prefix-p "git://" url)
                (format "git://%s/%s.git" host fork))
               ((string-prefix-p "http://" url)
                (format "http://%s/%s.git" host fork))
               ((error "%s has an unexpected format" url))))))))

;;;###autoload
(defun forge-checkout-pullreq (pullreq)
  "Create, configure and checkout a new branch from a pull-request.
Please see the manual for more information."
  (interactive (list (forge-read-pullreq "Checkout pull request")))
  (magit--checkout (forge--branch-pullreq (forge-get-pullreq pullreq)))
  (forge-refresh-buffer))

;;;###autoload(autoload 'forge-checkout-this-pullreq "forge-commands" nil t)
(transient-define-suffix forge-checkout-this-pullreq ()
  "Checkout the current pull-request.
If the branch for that pull-request does not exist yet, then create and
configure it first."
  :description "checkout"
  :inapt-if-not #'forge-current-pullreq
  (interactive)
  (forge-checkout-pullreq (forge-current-topic t)))

;;;###autoload
(defun forge-checkout-worktree (path pullreq)
  "Create, configure and checkout a new worktree from a pull-request.
This is like `forge-checkout-pullreq', except that it also
creates a new worktree. Please see the manual for more
information."
  (interactive
   (let ((id (forge-read-pullreq "Checkout pull request")))
     (list (funcall forge-checkout-worktree-read-directory-function
                    (forge-get-pullreq id))
           id)))
  (when (and (file-exists-p path)
             (not (and (file-directory-p path)
                       (length= (directory-files path) 2))))
    (user-error "%s already exists and isn't empty" path))
  (magit-worktree-checkout path
                           (forge--branch-pullreq (forge-get-pullreq pullreq))))

(defun forge-checkout-worktree-default-read-directory-function (pullreq)
  (pcase-let* (((eieio number head-ref) pullreq)
               (path (read-directory-name
                      (format "Checkout #%s in new worktree: " number)
                      (file-name-directory
                       (directory-file-name default-directory))
                      nil nil
                      (let ((branch (forge--pullreq-branch-internal pullreq)))
                        (if (string-match-p "\\`pr-[0-9]+\\'" branch)
                            (number-to-string number)
                          (format "%s-%s" number
                                  (string-replace "/" "-" head-ref)))))))
    (when (equal path "")
      (user-error "The empty string isn't a valid path"))
    path))

;;; Marks

(defun forge-create-mark (name face description)
  "Define a new mark that topics can be marked with."
  (interactive
   (list (read-string "Name: ")
         (magit-read-char-case "Set appearance using " nil
           (?n "a face [n]ame"
               (read-face-name "Face name: "))
           (?s "face [s]exp"
               (read-from-minibuffer
                "Face sexp: "
                "(:background \"\" :foreground \"\" :box t)"
                read-expression-map t)))
         (let ((str (read-string "Description: ")))
           (and (not (equal str "")) str))))
  (forge-sql [:insert-into mark :values $v1]
             (vector nil (forge--uuid) name face description)))

(defun forge-edit-mark (id name face description)
  "Define a new mark that topics can be marked with."
  (interactive
   (pcase-let*
       ((marks (forge-sql [:select [name id face description] :from mark]))
        (`(,name ,id ,face ,description)
         (assoc (completing-read "Edit mark" (mapcar #'car marks) nil t)
                marks)))
     (list id
           (read-string "Name: " name)
           (magit-read-char-case "Set appearance using " nil
             (?n "a face [n]ame"
                 (read-face-name "Face name: " (and (symbolp face) face)))
             (?s "face [s]exp"
                 (read-from-minibuffer
                  "Face sexp: "
                  (if (listp face)
                      (format "%S" face)
                    "(:background \"\" :foreground \"\" :box t)")
                  read-expression-map t)))
           (let ((str (read-string "Description: " nil nil description)))
             (and (not (equal str "")) str)))))
  (forge-sql [:update mark
              :set (= [name face description] $v1)
              :where (= id $s2)]
             (vector name face description) id))

;;; Remotely

;;;###autoload
(defun forge-fork (fork remote)
  "Fork the current repository to FORK and add it as a REMOTE.
If the fork already exists, then that isn't an error; the remote
is added anyway.  Currently this only supports Github and Gitlab."
  (interactive
   (let ((fork (magit-completing-read "Fork to"
                                      (mapcar #'car forge-owned-accounts))))
     (list fork
           (read-string "Remote name: "
                        (or (plist-get (cdr (assoc fork forge-owned-accounts))
                                       'remote-name)
                            fork)))))
  (let ((repo (forge-get-repository :stub)))
    (forge--fork-repository repo fork)
    (magit-remote-add remote
                      (magit-clone--format-url (oref repo githost) fork
                                               (oref repo name))
                      (list "--fetch"))))

;;;###autoload(autoload 'forge-merge "forge-commands" nil t)
(transient-define-suffix forge-merge (pullreq method)
  "Merge the current pull-request using METHOD using the forge's API.

If there is no current pull-request or with a prefix argument,
then read pull-request PULLREQ to visit instead.

Use of this command is discouraged.  Unless the remote repository
is configured to disallow that, you should instead merge locally
and then push the target branch.  Forges detect that you have
done that and respond by automatically marking the pull-request
as merged."
  (interactive
   (list (forge-read-pullreq "Merge pull-request")
         (if (forge--childp (forge-get-repository :tracked)
                            'forge-gitlab-repository)
             (magit-read-char-case "Merge method " t
               (?m "[m]erge"  'merge)
               (?s "[s]quash" 'squash))
           (magit-read-char-case "Merge method " t
             (?m "[m]erge"  'merge)
             (?s "[s]quash" 'squash)
             (?r "[r]ebase" 'rebase)))))
  (let ((pullreq (forge-get-pullreq pullreq)))
    (forge--merge-pullreq (forge-get-repository pullreq)
                          pullreq
                          (magit-rev-hash
                           (forge--pullreq-branch-internal pullreq))
                          method))
  (forge-pull))

;;;###autoload
(defun forge-rename-default-branch ()
  "Rename the default branch to NEWNAME.
Change the name on the upstream remotely and locally, and update the
upstream remotes of local branches accordingly."
  (interactive)
  (let* ((repo (forge-get-repository :tracked?))
         (_ (unless (forge-github-repository-p repo)
              (user-error "Updating default branch not supported for forge `%s'"
                          (oref repo forge))))
         (remote (or (and (fboundp 'forge--get-remote)
                          (forge--get-remote))
                     (magit-get-some-remote)
                     (user-error "No remote configured")))
         (symref (format "refs/remotes/%s/HEAD" remote))
         (oldhead (progn
                    (message "Determining old default branch...")
                    (magit-git "fetch" "--prune")
                    (magit-git "remote" "set-head" "--auto" remote)
                    (message "Determining old default branch...done")
                    (magit-git-string "symbolic-ref" "--short" symref)))
         (oldname (if oldhead
                      (cdr (magit-split-branch-name oldhead))
                    (error "Cannot determine old default branch")))
         (default (and (not (equal oldname "main")) "main"))
         (newname (read-string
                   (format "Rename default branch `%s' to%s: "
                           oldname
                           (if default (format " (default: %s)" default) ""))
                   nil nil default)))
    (message "Renaming default branch...")
    (forge--set-default-branch repo newname oldname)
    (forge-refresh-buffer)
    (message "Renaming default branch...done")))

;;; Configuration

(transient-define-suffix forge-forge.remote ()
  "Change the local value of the `forge.remote' Git variable."
  :class 'magit--git-variable:choices
  :variable "forge.remote"
  :choices #'magit-list-remotes
  :default (lambda (_) (forge--get-remote t t))
  (interactive)
  (let ((obj (transient-suffix-object)))
    (transient-infix-set obj (transient-infix-read obj)))
  (if (and transient--prefix
           (eq (oref transient--prefix command) 'forge-add-repository))
      ;; Improvements to Transient will make this hack unnecessary.
      (let ((scope (forge-add-repository--scope)))
        (oset (transient-prefix-object) scope scope))
    (transient--show)))

(transient-define-infix forge-forge.graphqlItemLimit ()
  "Change the maximum number of GraphQL entities to pull at once."
  :if #'forge--get-github-repository
  :class 'magit--git-variable
  :variable "forge.graphqlItemLimit"
  :reader #'read-string
  :default (lambda () (number-to-string ghub-graphql-items-per-request)))

(transient-define-suffix forge-toggle-display-in-status-buffer ()
  "Toggle whether to display topics in the current status buffer."
  :if-mode 'magit-status-mode
  :inapt-if-not #'forge--buffer-with-topics-sections-p
  :description (lambda ()
                 (if (and forge--buffer-topics-spec
                          (oref forge--buffer-topics-spec type))
                     "hide topics"
                   "display topics"))
  (interactive)
  (oset forge--buffer-topics-spec type
        (if (oref forge--buffer-topics-spec type) nil 'topic))
  (forge-refresh-buffer))

(defun forge--buffer-with-topics-sections-p ()
  (and forge--buffer-topics-spec
       (not (eq major-mode 'forge-topics-mode))
       (forge-get-repository :tracked?)))

;;;###autoload(autoload 'forge-add-pullreq-refspec "forge-commands" nil t)
(transient-define-suffix forge-add-pullreq-refspec ()
  "Configure Git to fetch all pull-requests.
This is done by adding \"+refs/pull/*/head:refs/pullreqs/*\"
to the value of `remote.REMOTE.fetch', where REMOTE is the
upstream remote."
  :if-not 'forge--pullreq-refspec
  :description "add pull-request refspec"
  (interactive)
  (let* ((repo    (forge-get-repository :stub))
         (remote  (oref repo remote))
         (fetch   (magit-get-all "remote" remote "fetch"))
         (refspec (oref repo pullreq-refspec)))
    (if (member refspec fetch)
        (message "Pull-request refspec is already active")
      (magit-call-git "config" "--add"
                      (format "remote.%s.fetch" remote)
                      refspec)
      (magit-git-fetch remote (magit-fetch-arguments)))))

(defun forge--pullreq-refspec ()
  (let* ((repo    (forge-get-repository :stub))
         (remote  (oref repo remote))
         (fetch   (magit-get-all "remote" remote "fetch"))
         (refspec (oref repo pullreq-refspec)))
    (car (member refspec fetch))))

;;; Add repositories

;;;###autoload(autoload 'forge-add-repository "forge-commands" nil t)
(transient-define-prefix forge-add-repository (&optional repo limit)
  "Add a repository to the database."
  :refresh-suffixes t
  [:class transient-subgroups

   ;; Already tracked.
   [:if (lambda () (forge--scope :tracked))
    (:info*
     (lambda ()
       (format
        (propertize "%s is already being tracked" 'face 'transient-heading)
        (propertize (forge--scope 'url) 'face 'bold)))
     :format "%d")]

   ;; Nothing to tracked.
   [:if-not (lambda () (forge--scope 'topdir))
    (:info*
     (lambda ()
       (format
        (propertize "%s is not inside a Git repository" 'face 'transient-heading)
        (propertize default-directory 'face 'bold)))
     :format "%d")]

   ;; Cannot track.
   [:if (lambda () (and (not (forge--scope 'repo)) (forge--scope 'topdir)))
    :description
    (lambda ()
      (concat
       (format (propertize "Cannot determine forge host for %s\n"
                           'face 'transient-heading)
               (propertize (forge--scope 'topdir) 'face 'bold))
       (if-let* ((remote (forge--get-remote))
                 (url (magit-git-string "remote" "get-url" remote)))
             (format (propertize "because %s is not on a host known to Forge."
                                 'face 'transient-heading)
                     (propertize url 'face 'bold))
           (propertize "because no suitable remote was detected."
                       'face 'transient-heading))))
    ("r" forge-forge.remote :format " %k Try another %d %v" :face 'bold)
    ("h" "Learn how to configure another Github host"
     (lambda () (interactive) (info "(forge)Setup for Another Github Instance")))
    ("l" "Learn how to configure another Gitlab host"
     (lambda () (interactive) (info "(forge)Setup for Another Gitlab Instance")))
    ("p" "Learn how to configure partially supported host"
     (lambda () (interactive) (info "(forge)Setup a Partially Supported Host")))]

   ;; Track it!
   [:if (lambda () (forge--scope :untracked))
    :description
    (lambda ()
      (format
       (propertize "Adding %s to database," 'face 'transient-heading)
       (propertize (forge--scope 'url) 'face 'bold)))
    ("r" forge-forge.remote :format " %k from %d %v," :face 'bold)
    ("a" "pulling all topics"
     (lambda (repo)
       (interactive (list (forge--scope 'repo)))
       (forge-add-repository repo)))
    ("s" "pulling only topics since <date>"
     (lambda (repo date)
       (interactive
        (list (forge--scope 'repo)
              (forge-read-date "Limit pulling to topics updated since: ")))
       (forge-add-repository repo date)))
    ("i" "to allow pulling of individual topics"
     (lambda (repo)
       (interactive (list (forge--scope 'repo)))
       (forge-add-repository repo :selective)))]

   ;; Pivot.
   [("o" "Add another repository" forge-add-some-repository)
    (7 "U" "Add all source repositories belonging to a user"
       forge-add-user-repositories)
    (7 "O" "Add all source repositories belonging to an organization"
       forge-add-organization-repositories)]]
  (declare (interactive-only nil))
  (interactive)
  (cond
   ((not repo)
    (transient-setup 'forge-add-repository nil nil
                     :scope (forge-add-repository--scope)))
   ((stringp repo)
    (transient-setup 'forge-add-repository nil nil
                     :scope (forge-add-repository--scope repo)))
   (t
    (when-let*
        (((not (eq limit :selective)))
         ((magit-git-config-p "forge.autoPull" t))
         (remote  (oref repo remote))
         (refspec (oref repo pullreq-refspec))
         (default-directory (forge-get-worktree repo))
         ((and (not (member refspec (magit-get-all "remote" remote "fetch")))
               (or (eq forge-add-pullreq-refspec t)
                   (and (eq forge-add-pullreq-refspec 'ask)
                        (y-or-n-p (format "Also add %S refspec? " refspec)))))))
      (magit-call-git "config" "--add"
                      (format "remote.%s.fetch" remote)
                      refspec))
    (setq repo (forge-get-repository repo nil :insert!))
    (when (eq limit :selective)
      (oset repo selective-p t)
      (setq limit nil))
    (forge--pull repo
                 (and (not (forge-get-worktree repo)) #'ignore)
                 limit))))

(defun forge-add-repository--scope (&optional directory)
  (let* ((repo      (if directory
                        (forge-get-repository directory nil :stub?)
                      (forge-get-repository :stub?)))
         (wtree     (and repo (forge-get-worktree repo)))
         (condition (and repo (oref repo condition)))
         (val
          `((repo       . ,repo)
            (wtree      . ,wtree)
            (condition  . ,condition)
            (:tracked   . ,(eq condition :tracked))
            (:untracked . ,(memq condition '(:known :stub)))
            (topdir     . ,(or wtree (magit-toplevel)))
            (url        . ,(and repo (forge-get-url repo))))))
    val))

(defun forge--scope (&optional key)
  ;; `transient-scope' itself probably offer optional KEY.
  (let ((scope (transient-scope)))
    (if key (alist-get key scope) scope)))

(defun forge-add-some-repository (url)
  "Read a repository and add it to the database."
  (interactive
   (let (ret url)
     (while (not ret)
       (setq url (magit-read-string-ns
                  "Add repository to database (url, owner/name, or name)" url))
       (unless (string-match-p "\\(://\\|@\\)" url)
         (setq url (magit-clone--name-to-url url)))
       (cond ((forge-get-repository url nil :tracked?)
              (message "%s is already being tracked locally"
                       (propertize url 'face 'bold))
              (sit-for 3))
             ((not (forge-get-repository url nil :valid?))
              (message "%s does not exist or is inaccessible"
                       (propertize url 'face 'bold))
              (sit-for 3))
             ((setq ret url))))
     (list ret)))
  (forge-add-repository url))

;;;###autoload
(defun forge-add-user-repositories (host user)
  "Add all of USER's repositories from HOST to the database.
This may take a while.  Only Github is supported at the moment."
  (interactive
   (list (forge-read-host "Add repositories from Github host"
                          'forge-github-repository)
         (read-string "User: ")))
  (forge--add-user-repos 'forge-github-repository host user))

;;;###autoload
(defun forge-add-organization-repositories (host organization)
  "Add all of ORGANIZATION's repositories from HOST to the database.
This may take a while.  Only Github is supported at the moment."
  (interactive
   (list (forge-read-host "Add repositories from Github host"
                          'forge-github-repository)
         (read-string "Organization: ")))
  (forge--add-organization-repos 'forge-github-repository host organization))

;;; Cleanup

;;;###autoload
(defun forge-remove-repository (repository)
  "Remove a repository from the database."
  (interactive
   (pcase-let* ((repo (forge-read-repository "Remove repository from db"))
                ((eieio githost owner name) repo))
     (if (yes-or-no-p (format "Do you really want to remove \"%s/%s @%s\" %s? "
                              owner name githost "from the database"))
         (list repo)
       (user-error "Abort"))))
  (closql-delete repository)
  (forge-refresh-buffer))

;;;###autoload
(defun forge-remove-topic-locally (topic)
  "Remove a topic from the local database only.
Due to how the supported APIs work, it would be too expensive to
automatically remove topics from the local database that were
removed from the forge.  The purpose of this command is to allow
you to manually clean up the local database."
  (interactive (list (forge-read-topic "Delete topic LOCALLY only")))
  (setq topic (forge-get-topic topic))
  (closql-delete topic)
  (if (and (derived-mode-p 'forge-topic-mode)
           (eq (oref topic id)
               (oref forge-buffer-topic id)))
      (kill-buffer (current-buffer))
    (forge-refresh-buffer)))

;;;###autoload
(defun forge-reset-database ()
  "Move the current database file to the trash.
This is useful after the database scheme has changed, which will
happen a few times while the forge functionality is still under
heavy development."
  (interactive)
  (when (and (file-exists-p forge-database-file)
             (yes-or-no-p "Really trash Forge's database file? "))
    (when-let ((db (forge-db t)))
      (emacsql-close db))
    (delete-file forge-database-file t)
    (forge-refresh-buffer)))

;;; Miscellaneous

(magit-define-section-jumper forge-jump-to-pullreqs "Pull requests" pullreqs)
(magit-define-section-jumper forge-jump-to-issues "Issues" issues)

;;; _
(provide 'forge-commands)
;;; forge-commands.el ends here
