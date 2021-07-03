;;; forge-commands.el --- Commands                 -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; Forge is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Forge is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Forge.  If not, see http://www.gnu.org/licenses.

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

;;; Dispatch

;;;###autoload (autoload 'forge-dispatch "forge-commands" nil t)
(transient-define-prefix forge-dispatch ()
  "Dispatch a forge command."
  [["Fetch"
    ("f f" "all topics"    forge-pull)
    ("f t" "one topic"     forge-pull-topic)
    ("f n" "notifications" forge-pull-notifications)]
   ["List"
    ("l t" "topics"        forge-list-topics)
    ("l i" "issues"        forge-list-issues)
    ("l p" "pull-requests" forge-list-pullreqs)
    ("l n" "notifications" forge-list-notifications)
    ("l r" "repositories"  forge-list-repositories)
    (7 "l a" "awaiting review"        forge-list-requested-reviews)
    (7 "m i" "authored issues"        forge-list-authored-issues)
    (7 "m p" "authored pull-requests" forge-list-authored-pullreqs)
    (7 "o i" "owned issues"           forge-list-owned-issues)
    (7 "o p" "owned pull-requests"    forge-list-owned-pullreqs)
    (7 "o r" "owned repositories"     forge-list-owned-repositories)]
   ["Visit"
    ("v t" "topic"         forge-visit-topic)
    ("v i" "issue"         forge-visit-issue)
    ("v p" "pull-request"  forge-visit-pullreq)
    """Create"
    ("c i" "issue"         forge-create-issue)
    ("c p" "pull-request"  forge-create-pullreq)
    ("c u" "pull-request from issue" forge-create-pullreq-from-issue
     :if (lambda () (forge-github-repository-p (forge-get-repository nil))))
    ("c f" "fork or remote" forge-fork)]]
  [["Configure"
    ("a" "add repository to database" forge-add-repository)
    ("r" "forge.repository" forge-forge.remote)
    (7 "t" forge-toggle-display-in-status-buffer)]])

;;; Pull

;;;###autoload
(defun forge-pull (&optional repo until)
  "Pull topics from the forge repository.

With a prefix argument and if the repository has not been fetched
before, then read a date from the user and limit pulled topics to
those that have been updated since then.

If pulling is too slow, then also consider setting the Git variable
`forge.omitExpensive' to `true'."
  (interactive
   (list nil
         (and current-prefix-arg
              (not (forge-get-repository 'full))
              (forge-read-date "Limit pulling to topics updates since: "))))
  (forge--zap-repository-cache repo)
  (let (create)
    (unless repo
      (setq repo (forge-get-repository 'full))
      (unless repo
        (setq repo (forge-get-repository 'create))
        (setq create t)))
    (when (oref repo selective-p)
      (if (yes-or-no-p
           (format "Always pull all of %s/%s's topics going forward?"
                   (oref repo owner)
                   (oref repo name)))
          (oset repo selective-p nil)
        (user-error "Abort")))
    (setq forge--mode-line-buffer (current-buffer))
    (when-let ((remote  (oref repo remote))
               (refspec (oref repo pullreq-refspec)))
      (when (and create
                 (not (member refspec (magit-get-all "remote" remote "fetch")))
                 (or (eq forge-add-pullreq-refspec t)
                     (and (eq forge-add-pullreq-refspec 'ask)
                          (y-or-n-p (format "Also add %S refspec? " refspec)))))
        (magit-call-git "config" "--add"
                        (format "remote.%s.fetch" remote)
                        refspec)))
    (forge--msg repo t nil "Pulling REPO")
    (forge--pull repo until)))

(defun forge-read-date (prompt)
  (cl-block nil
    (while t
      (let ((str (read-from-minibuffer prompt)))
        (cond ((string-equal str "")
               (cl-return nil))
              ((string-match-p
                "\\`[0-9]\\{4\\}[-/][0-9]\\{2\\}[-/][0-9]\\{2\\}\\'" str)
               (cl-return str))))
      (message "Please enter a date in the format YYYY-MM-DD.")
      (sit-for 1))))

(cl-defmethod forge--pull ((repo forge-noapi-repository) _until) ; NOOP
  (forge--msg repo t t "Pulling from REPO is not supported"))

(cl-defmethod forge--pull ((repo forge-unusedapi-repository) _until)
  (oset repo sparse-p nil)
  (magit-git-fetch (oref repo remote) (magit-fetch-arguments)))

(defun forge--git-fetch (buf dir repo)
  (if (buffer-live-p buf)
      (with-current-buffer buf
        (magit-git-fetch (oref repo remote) (magit-fetch-arguments)))
    (let ((default-directory dir))
      (magit-git-fetch (oref repo remote) (magit-fetch-arguments)))))

;;;###autoload
(defun forge-pull-notifications ()
  "Fetch notifications for all repositories from the current forge."
  (interactive)
  (let* ((repo  (forge-get-repository 'stub))
         (class (eieio-object-class repo)))
    (if (eq class 'forge-github-repository)
        (forge--pull-notifications class (oref repo githost))
      (user-error "Fetching notifications not supported for forge %S"
                  (oref repo forge)))))

;;;###autoload
(defun forge-pull-topic (n)
  "Pull the API data for the current topic.
If there is no current topic or with a prefix argument read a
topic N to pull instead."
  (interactive (list (forge-read-topic "Pull topic")))
  (forge--pull-topic (forge-get-repository t) n))

(cl-defmethod forge--pull-topic ((repo forge-repository) _n)
  (error "Fetching an individual topic not implemented for %s"
         (eieio-object-class repo)))

(defun forge--zap-repository-cache (&optional repo)
  (when-let ((r (if repo
                    (oref repo worktree)
                  (magit-repository-local-repository))))
    (magit-repository-local-delete (list 'forge-ls-recent-topics 'issue) r)
    (magit-repository-local-delete (list 'forge-ls-recent-topics 'pullreq) r)))

;;; Browse

;;;###autoload
(defun forge-browse-dwim ()
  "Visit a topic, branch or commit using a browser.
Prefer a topic over a branch and that over a commit."
  (interactive)
  (if-let ((topic (forge-topic-at-point)))
      (forge-browse topic)
    (if-let ((branch (magit-branch-at-point)))
        (forge-browse-branch branch)
      (call-interactively 'forge-browse-commit))))

;;;###autoload
(defun forge-browse-commit (rev)
  "Visit the url corresponding to REV using a browser."
  (interactive
   (list (or (magit-completing-read "Browse commit"
                                    (magit-list-branch-names)
                                    nil nil nil 'magit-revision-history
                                    (magit-branch-or-commit-at-point))
             (user-error "Nothing selected"))))
  (let ((repo (forge-get-repository 'stub)))
    (unless (magit-list-containing-branches
             rev "-r" (concat (oref repo remote) "/*"))
      (if-let ((branch (car (magit-list-containing-branches rev "-r"))))
          (setq repo (forge-get-repository
                      'stub (cdr (magit-split-branch-name branch))))
        (message "%s does not appear to be available on any remote.  %s"
                 rev "You might have to push it first.")))
    (browse-url
     (forge--format repo 'commit-url-format
                    `((?r . ,(magit-commit-p rev)))))))

;;;###autoload
(defun forge-copy-url-at-point-as-kill ()
  "Copy the url of the thing at point."
  (interactive)
  (if-let ((url (forge-get-url (or (forge-post-at-point)
                                   (forge-current-topic)))))
      (progn
        (kill-new url)
        (message "Copied %S" url))
    (user-error "Nothing at point with a URL")))

;;;###autoload
(defun forge-browse-branch (branch)
  "Visit the url corresponding BRANCH using a browser."
  (interactive (list (magit-read-branch "Browse branch")))
  (let (remote)
    (if (magit-remote-branch-p branch)
        (let ((cons (magit-split-branch-name branch)))
          (setq remote (car cons))
          (setq branch (cdr cons)))
      (or (setq remote (or (magit-get-push-remote branch)
                           (magit-get-upstream-remote branch)))
          (user-error "Cannot determine remote for %s" branch)))
    (browse-url (forge--format remote 'branch-url-format
                               `((?r . ,branch))))))

;;;###autoload
(defun forge-browse-remote (remote)
  "Visit the url corresponding to REMOTE using a browser."
  (interactive (list (magit-read-remote "Browse remote")))
  (browse-url (forge--format remote 'remote-url-format)))

;;;###autoload
(defun forge-browse-topic ()
  "Visit the current topic using a browser."
  (interactive)
  (if-let ((topic (forge-current-topic)))
      (forge-browse topic)
    (user-error "There is no current topic")))

;;;###autoload
(defun forge-browse-pullreqs ()
  "Visit the pull-requests of the current repository using a browser."
  (interactive)
  (browse-url (forge--format (forge-get-repository 'stub)
                             'pullreqs-url-format)))

;;;###autoload
(defun forge-browse-pullreq (n)
  "Visit the url corresponding to pullreq N using a browser."
  (interactive (list (forge-read-pullreq "Browse pull-request" t)))
  (forge-browse (forge-get-pullreq n)))

;;;###autoload
(defun forge-browse-issues ()
  "Visit the issues of the current repository using a browser."
  (interactive)
  (browse-url (forge--format (forge-get-repository 'stub)
                             'issues-url-format)))

;;;###autoload
(defun forge-browse-issue (n)
  "Visit the current issue using a browser.
If there is no current issue or with a prefix argument
read an issue N to visit."
  (interactive (list (forge-read-issue "Browse issue" t)))
  (forge-browse (forge-get-issue n)))

;;;###autoload
(defun forge-browse-post ()
  "Visit the current post using a browser."
  (interactive)
  (if-let ((post (forge-post-at-point)))
      (forge-browse post)
    (user-error "There is no current post")))

;;; Visit

;;;###autoload
(defun forge-visit-topic (topic)
  "View the current topic in a separate buffer.
If there is no current topic or with a prefix argument
read a topic to visit instead."
  (interactive (list (or (forge-current-topic)
                         (forge-get-topic
                          (forge-read-topic "View topic")))))
  (forge-visit topic))

;;;###autoload
(defun forge-visit-pullreq (n)
  "View the current pull-request in a separate buffer.
If there is no current pull-request or with a prefix argument
read pull-request N to visit instead."
  (interactive (list (forge-read-pullreq "View pull-request" t)))
  (forge-visit (forge-get-pullreq n)))

;;;###autoload
(defun forge-visit-issue (n)
  "Visit the current issue in a separate buffer.
If there is no current issue or with a prefix argument
read an issue N to visit instead."
  (interactive (list (forge-read-issue "View issue" t)))
  (forge-visit (forge-get-issue n)))

;;;###autoload
(defun forge-visit-repository (repo)
  "View the current repository in a separate buffer."
  (interactive
   (list (or (forge-current-repository)
             (forge-get-repository
              (forge-read-repository "Visit repository")))))
  (forge-visit repo))

;;; Create

(defun forge-create-pullreq (source target)
  "Create a new pull-request for the current repository."
  (interactive (forge-create-pullreq--read-args))
  (let* ((repo (forge-get-repository t))
         (buf (forge--prepare-post-buffer
               "new-pullreq"
               (forge--format repo "Create new pull-request on %p")
               source target)))
    (with-current-buffer buf
      (setq forge--buffer-base-branch target)
      (setq forge--buffer-head-branch source)
      (setq forge--buffer-post-object repo)
      (setq forge--submit-post-function 'forge--submit-create-pullreq))
    (forge--display-post-buffer buf)))

(defun forge-create-pullreq-from-issue (n source target)
  "Convert an existing issue into a pull-request."
  (interactive (cons (forge-read-issue "Convert issue")
                     (forge-create-pullreq--read-args)))
  (forge--create-pullreq-from-issue (forge-get-repository t)
                                    n source target))

(defun forge-create-pullreq--read-args ()
  (let* ((source  (magit-completing-read
                   "Source branch"
                   (magit-list-remote-branch-names)
                   nil t nil 'magit-revision-history
                   (or (when-let ((d (magit-branch-at-point)))
                         (if (magit-remote-branch-p d)
                             d
                           (magit-get-push-branch d t)))
                       (when-let ((d (magit-get-current-branch)))
                         (if (magit-remote-branch-p d)
                             d
                           (magit-get-push-branch d t))))))
         (repo    (forge-get-repository t))
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

(defun forge-create-issue ()
  "Create a new issue for the current repository."
  (interactive)
  (let* ((repo (forge-get-repository t))
         (buf (forge--prepare-post-buffer
               "new-issue"
               (forge--format repo "Create new issue on %p"))))
    (when buf
      (with-current-buffer buf
        (setq forge--buffer-post-object repo)
        (setq forge--submit-post-function 'forge--submit-create-issue))
      (forge--display-post-buffer buf))))

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
      (setq forge--submit-post-function 'forge--submit-create-post)
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
  (let* ((post (or (forge-post-at-point)
                   (user-error "There is no current post")))
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
      (setq forge--submit-post-function 'forge--submit-edit-post)
      (erase-buffer)
      (when (cl-typep post 'forge-topic)
        (insert "# " (oref post title) "\n\n"))
      (insert (oref post body)))
    (forge--display-post-buffer buf)))

(defun forge-edit-topic-title (n)
  "Edit the title of the current topic.
If there is no current topic or with a prefix argument read a
topic N and modify that instead."
  (interactive (list (forge-read-topic "Edit title of")))
  (let ((topic (forge-get-topic n)))
    (forge--set-topic-title
     (forge-get-repository topic) topic
     (read-string "Title: " (oref topic title)))))

(defun forge-edit-topic-state (n)
  "Close or reopen the current topic.
If there is no current topic or with a prefix argument read a
topic N and modify that instead."
  (interactive
   (let* ((n (forge-read-topic "Close/reopen"))
          (topic (forge-get-topic n)))
     (if (magit-y-or-n-p
          (format "%s %S"
                  (cl-ecase (oref topic state)
                    (merged (error "Merged pull-requests cannot be reopened"))
                    (closed "Reopen")
                    (open   "Close"))
                  (forge--topic-format-choice topic)))
         (list n)
       (user-error "Abort"))))
  (let ((topic (forge-get-topic n)))
    (forge--set-topic-state (forge-get-repository topic) topic)))

(defun forge-edit-topic-milestone (n)
  (interactive (list (forge-read-topic "Edit milestone of")))
  (let* ((topic (forge-get-topic n))
         (repo  (forge-get-repository topic)))
    (forge--set-topic-milestone
     repo topic
     (magit-completing-read
      "Milestone"
      (mapcar #'caddr (oref repo milestones))
      nil t (forge--get-topic-milestone topic)))))

(defun forge-edit-topic-labels (n)
  "Edit the labels of the current topic.
If there is no current topic or with a prefix argument read a
topic N and modify that instead."
  (interactive (list (forge-read-topic "Edit labels of")))
  (let* ((topic (forge-get-topic n))
         (repo  (forge-get-repository topic))
         (crm-separator ","))
    (forge--set-topic-labels
     repo topic (magit-completing-read-multiple*
                 "Labels: "
                 (mapcar #'cadr (oref repo labels))
                 nil t
                 (mapconcat #'car (closql--iref topic 'labels) ",")))))

(defun forge-edit-topic-marks (n marks)
  "Edit the marks of the current topic.
If there is no current topic or with a prefix argument read a
topic N and modify that instead."
  (interactive
   (let ((n (forge-read-topic "Edit marks of")))
     (list n (forge-read-marks "Marks: " (forge-get-topic n)))))
  (oset (forge-get-topic n) marks marks)
  (magit-refresh))

(defun forge-edit-topic-assignees (n)
  "Edit the assignees of the current topic.
If there is no current topic or with a prefix argument read a
topic N and modify that instead."
  (interactive (list (forge-read-topic "Edit assignees of")))
  (let* ((topic (forge-get-topic n))
         (repo  (forge-get-repository topic))
         (value (closql--iref topic 'assignees))
         (choices (mapcar #'cadr (oref repo assignees)))
         (crm-separator ","))
    (forge--set-topic-assignees
     repo topic
     (if (and (forge--childp topic 'forge-pullreq)
              (forge--childp repo  'forge-gitlab-repository))
         (list ; Gitlab merge-requests can only be assigned to a single user.
          (magit-completing-read
           "Assignee" choices nil
           nil ; Empty input removes assignee.
           (car value)))
       (magit-completing-read-multiple*
        "Assignees: " choices nil
        (if (forge--childp repo 'forge-gitlab-repository)
            t ; Selecting something else would fail later on.
          'confirm)
        (mapconcat #'car value ","))))))

(defun forge-edit-topic-review-requests (n)
  "Edit the review-requests of the current pull-request.
If there is no current topic or with a prefix argument read a
topic N and modify that instead."
  (interactive (list (forge-read-pullreq "Request review for")))
  (let* ((topic (forge-get-pullreq n))
         (repo  (forge-get-repository topic))
         (value (closql--iref topic 'review-requests))
         (choices (mapcar #'cadr (oref repo assignees)))
         (crm-separator ","))
    (forge--set-topic-review-requests
     repo topic
     (magit-completing-read-multiple*
      "Request review from: " choices nil
      'confirm
      (mapconcat #'car value ",")))))

(defun forge-edit-topic-note (n)
  "Edit your private note about the current topic.
If there is no current topic or with a prefix argument read a
topic N and modify that instead."
  (interactive (list (forge-read-topic "Edit note about")))
  (let* ((topic (forge-get-topic n))
         (buf (forge--prepare-post-buffer
               (forge--format topic "%i;note")
               (forge--format topic "New note on #%i of %p"))))
    (with-current-buffer buf
      (setq forge--buffer-post-object topic)
      (setq forge--submit-post-function 'forge--save-note)
      (erase-buffer)
      (when-let ((note (oref topic note)))
        (save-excursion (insert note ?\n))))
    (forge--display-post-buffer buf)))

;;; Delete

(defun forge-delete-comment (comment)
  "Delete the comment at point."
  (interactive (list (or (forge-comment-at-point)
                         (user-error "There is no comment at point"))))
  (when (yes-or-no-p "Do you really want to delete the selected comment? ")
    (forge--delete-comment (forge-get-repository t) comment)))

;;; Branch

;;;###autoload
(defun forge-branch-pullreq (n)
  "Create and configure a new branch from a pull-request.
Please see the manual for more information."
  (interactive (list (forge-read-pullreq "Branch pull request" t)))
  (if-let ((branch (forge--pullreq-branch-active (forge-get-pullreq n))))
      (progn (message "Branch %S already exists and is configured" branch)
             branch)
    (forge--branch-pullreq (forge-get-repository t) n)))

(cl-defmethod forge--branch-pullreq ((_repo forge-unusedapi-repository) n)
  ;; We don't know enough to do a good job.
  (let ((branch (format "pr-%s" n)))
    (when (magit-branch-p branch)
      (user-error "Branch `%s' already exists" branch))
    (magit-git "branch" branch (forge--pullreq-ref n))
    ;; More often than not this is the correct target branch.
    (magit-call-git "branch" branch "--set-upstream-to=master")
    (magit-set (number-to-string n) "branch" branch "pullRequest")
    (magit-refresh)
    branch))

(cl-defmethod forge--branch-pullreq ((repo forge-repository) n)
  (with-slots (number title editable-p cross-repo-p state
                      base-ref base-repo
                      head-ref head-repo head-user)
      (forge-get-pullreq repo n)
    (let* ((host (oref repo githost))
           (upstream (oref repo remote))
           (upstream-url (magit-git-string "remote" "get-url" upstream))
           (remote head-user)
           (branch (forge--pullreq-branch-select (forge-get-pullreq repo n)))
           (pr-branch head-ref))
      (when (string-match-p ":" pr-branch)
        ;; Such a branch name would be invalid.  If we encounter
        ;; it anyway, then that means that the source branch and
        ;; the merge-request ref are missing.
        (error "Cannot check out this Gitlab merge-request \
because the source branch has been deleted"))
      (if (not (eq state 'open))
          (magit-git "branch" "--force" branch
                     (format "refs/pullreqs/%s" number))
        (if (not cross-repo-p)
            (let ((tracking (concat upstream "/" pr-branch)))
              (unless (magit-branch-p tracking)
                (magit-call-git "fetch" upstream))
              (magit-call-git "branch" branch tracking)
              (magit-branch-maybe-adjust-upstream branch tracking)
              (magit-set upstream "branch" branch "pushRemote")
              (magit-set upstream "branch" branch "pullRequestRemote"))
          (if (magit-remote-p remote)
              (let ((url   (magit-git-string "remote" "get-url" remote))
                    (fetch (magit-get-all "remote" remote "fetch")))
                (unless (forge--url-equal
                         url (format "git@%s:%s.git" host head-repo))
                  (user-error
                   "Remote `%s' already exists but does not point to %s"
                   remote url))
                (unless (or (member (format "+refs/heads/*:refs/remotes/%s/*"
                                            remote)
                                    fetch)
                            (member (format "+refs/heads/%s:refs/remotes/%s/%s"
                                            pr-branch remote pr-branch)
                                    fetch))
                  (magit-git "remote" "set-branches" "--add" remote pr-branch)
                  (magit-git "fetch" remote)))
            (magit-git
             "remote" "add" "-f" "--no-tags"
             "-t" pr-branch remote
             (cond ((or (string-prefix-p "git@" upstream-url)
                        (string-prefix-p "ssh://git@" upstream-url))
                    (format "git@%s:%s.git" host head-repo))
                   ((string-prefix-p "https://" upstream-url)
                    (format "https://%s/%s.git" host head-repo))
                   ((string-prefix-p "git://" upstream-url)
                    (format "git://%s/%s.git" host head-repo))
                   ((string-prefix-p "http://" upstream-url)
                    (format "http://%s/%s.git" host head-repo))
                   (t (error "%s has an unexpected format" upstream-url)))))
          (magit-git "branch" "--force" branch (concat remote "/" pr-branch))
          (if (and editable-p
                   (equal branch pr-branch))
              (magit-set remote "branch" branch "pushRemote")
            (magit-set upstream "branch" branch "pushRemote")))
        (magit-set remote "branch" branch "pullRequestRemote")
        (magit-set "true" "branch" branch "rebase")
        (magit-git "branch" branch
                   (concat "--set-upstream-to="
                           (if magit-branch-prefer-remote-upstream
                               (concat upstream "/" base-ref)
                             base-ref))))
      (magit-set (number-to-string number) "branch" branch "pullRequest")
      (magit-set title                     "branch" branch "description")
      (magit-refresh)
      branch)))

;;;###autoload
(defun forge-checkout-pullreq (n)
  "Create, configure and checkout a new branch from a pull-request.
Please see the manual for more information."
  (interactive (list (forge-read-pullreq "Checkout pull request" t)))
  (let ((pullreq (forge-get-pullreq n)))
    (magit-checkout
     (or (if (not (eq (oref pullreq state) 'open))
             (magit-ref-p (format "refs/pullreqs/%s"
                                  (oref pullreq number)))
           (forge--pullreq-branch-active pullreq))
         (let ((magit-inhibit-refresh t))
           (forge-branch-pullreq n))))))

;;;###autoload
(defun forge-checkout-worktree (path n)
  "Create, configure and checkout a new worktree from a pull-request.
This is like `forge-checkout-pullreq', except that it also
creates a new worktree. Please see the manual for more
information."
  (interactive
   (let* ((n (forge-read-pullreq "Checkout pull request" t))
          (pullreq (forge-get-pullreq n)))
     (with-slots (number head-ref) pullreq
       (let ((path (read-directory-name
                    (format "Checkout #%s in new worktree: " number)
                    (file-name-directory
                     (directory-file-name default-directory))
                    nil nil
                    (let ((branch (forge--pullreq-branch-internal pullreq)))
                      (if (string-match-p "\\`pr-[0-9]+\\'" branch)
                          (number-to-string number)
                        (format "%s-%s" number head-ref))))))
         (when (equal path "")
           (user-error "The empty string isn't a valid path"))
         (list path n)))))
  (when (and (file-exists-p path)
             (not (and (file-directory-p path)
                       (= (length (directory-files "/tmp/testing/")) 2))))
    (user-error "%s already exists and isn't empty" path))
  (magit-worktree-checkout path
                           (let ((magit-inhibit-refresh t))
                             (forge-branch-pullreq n))))

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
   (pcase-let ((`(,id ,name ,face ,description)
                (forge-read-mark "Edit mark")))
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

(defun forge-read-mark (prompt)
  "Read a topic.  Return (ID NAME FACE DESCRIPTION)."
  (let* ((marks (forge-sql [:select [id name face description] :from mark]))
         (name (completing-read prompt (mapcar #'cadr marks) nil t)))
    (--first (equal (cadr it) name) marks)))

(defun forge-read-marks (prompt &optional topic)
  "Read multiple mark names and return the respective ids."
  (let ((marks (forge-sql [:select [name id] :from mark]))
        (crm-separator ","))
    (--map (cadr (assoc it marks))
           (magit-completing-read-multiple*
            prompt (mapcar #'car marks) nil t
            (and topic
                 (mapconcat #'car (closql--iref topic 'marks) ","))))))

(defun forge-toggle-mark (mark)
  "Toggle MARK for the current topic."
  (if-let ((topic (forge-current-topic)))
      (let* ((value (mapcar #'car (closql--iref topic 'marks)))
             (value (if (member mark value)
                        (delete mark value)
                      (cons mark value)))
             (marks (forge-sql [:select [name id] :from mark])))
        (oset topic marks (--map (cadr (assoc it marks)) value))
        (magit-refresh))
    (user-error "There is no topic at point")))

;;; Fork

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
  (let ((repo (forge-get-repository 'stub)))
    (forge--fork-repository repo fork)
    (magit-remote-add remote
                      (magit-clone--format-url (oref repo githost) fork
                                               (oref repo name))
                      (list "--fetch"))))

;;; Misc

(transient-define-infix forge-forge.remote ()
  :class 'magit--git-variable:choices
  :variable "forge.remote"
  :choices 'magit-list-remotes
  :default "origin")

;;;###autoload
(defun forge-list-notifications ()
  "List notifications."
  (interactive)
  (forge-notifications-setup-buffer))

(transient-define-suffix forge-toggle-display-in-status-buffer ()
  :description (lambda ()
                 (if forge-display-in-status-buffer
                     "hide topics"
                   "display topics"))
  "Toggle whether to display topics in the current Magit status buffer."
  (interactive)
  (setq forge-display-in-status-buffer (not forge-display-in-status-buffer))
  (magit-refresh))

(defun forge-toggle-closed-visibility ()
  "Toggle whether recently closed issues are shown."
  (interactive)
  (magit-repository-local-delete (list 'forge-ls-recent-topics 'issue))
  (magit-repository-local-delete (list 'forge-ls-recent-topics 'pullreq))
  (make-local-variable 'forge-topic-list-limit)
  (if (atom forge-topic-list-limit)
      (setq forge-topic-list-limit (cons forge-topic-list-limit 5))
    (setcdr forge-topic-list-limit (* -1 (cdr forge-topic-list-limit))))
  (magit-refresh))

;;;###autoload
(defun forge-add-pullreq-refspec ()
  "Configure Git to fetch all pull-requests.
This is done by adding \"+refs/pull/*/head:refs/pullreqs/*\"
to the value of `remote.REMOTE.fetch', where REMOTE is the
upstream remote.  Also fetch from REMOTE."
  (interactive)
  (let* ((repo    (forge-get-repository 'stub))
         (remote  (oref repo remote))
         (fetch   (magit-get-all "remote" remote "fetch"))
         (refspec (oref repo pullreq-refspec)))
    (if (member refspec fetch)
        (message "Pull-request refspec is already active")
      (magit-call-git "config" "--add"
                      (format "remote.%s.fetch" remote)
                      refspec)
      (magit-git-fetch remote (magit-fetch-arguments)))))

;;;###autoload
(defun forge-add-repository (url)
  "Add a repository to the database.
Offer to either pull topics (now and in the future) or to only
pull individual topics when the user invokes `forge-pull-topic'."
  (declare (interactive-only t))
  (interactive
   (let ((str (magit-read-string-ns
               "Add repository to database (url or name)"
               (when-let ((repo (forge-get-repository 'stub))
                          (remote (oref repo remote)))
                 (magit-git-string "remote" "get-url" remote)))))
     (if (string-match-p "\\(://\\|@\\)" str)
         (list str)
       (list (magit-clone--name-to-url str)))))
  (if (forge-get-repository url nil 'full)
      (user-error "%s is already tracked in Forge database" url)
    (let ((repo (forge-get-repository url nil 'create)))
      (oset repo sparse-p nil)
      (magit-read-char-case "Pull " nil
        (?a "[a]ll topics"
            (forge-pull repo))
        (?i "[i]ndividual topics (useful for casual contributors)"
            (oset repo selective-p t))))))

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

;;;###autoload
(defun forge-merge (n method)
  "Merge the current pull-request using METHOD using the forge's API.

If there is no current pull-request or with a prefix argument,
then read pull-request N to visit instead.

Use of this command is discouraged.  Unless the remote repository
is configured to disallow that, you should instead merge locally
and then push the target branch.  Forges detect that you have
done that and respond by automatically marking the pull-request
as merged."
  (interactive
   (list (forge-read-pullreq "Merge pull-request" t)
         (if (forge--childp (forge-get-repository t) 'forge-gitlab-repository)
             (magit-read-char-case "Merge method " t
               (?m "[m]erge"  'merge)
               (?s "[s]quash" 'squash))
           (magit-read-char-case "Merge method " t
             (?m "[m]erge"  'merge)
             (?s "[s]quash" 'squash)
             (?r "[r]ebase" 'rebase)))))
  (forge--merge-pullreq (forge-get-repository t)
                        (forge-get-pullreq n)
                        (magit-rev-hash
                         (forge--pullreq-branch-internal
                          (forge-get-pullreq n)))
                        method)
  (forge-pull))

;;;###autoload
(defun forge-remove-repository (host owner name)
  "Remove a repository from the database."
  (interactive
   (pcase-let ((`(,githost ,owner ,name)
                (forge-read-repository "Remove repository from db")))
     (if (yes-or-no-p
          (format "Do you really want to remove \"%s/%s @%s\" from the db? "
                  owner name githost))
         (list githost owner name)
       (user-error "Abort"))))
  (closql-delete (forge-get-repository (list host owner name)))
  (magit-refresh))

;;;###autoload
(defun forge-remove-topic-locally (n)
  "Remove a topic from the local database only.
Due to how the supported APIs work, it would be too expensive to
automatically remove topics from the local datbase that were
removed from the forge.  The purpose of this command is to allow
you to manually clean up the local database."
  (interactive (list (forge-read-topic "Delete topic LOCALLY only")))
  (closql-delete (forge-get-topic n))
  (if (and (derived-mode-p 'forge-topic-mode)
           (eq n (oref forge-buffer-topic number)))
      (kill-buffer (current-buffer))
    (magit-refresh)))

;;;###autoload
(defun forge-reset-database ()
  "Move the current database file to the trash.
This is useful after the database scheme has changed, which will
happen a few times while the forge functionality is still under
heavy development."
  (interactive)
  (when (and (file-exists-p forge-database-file)
             (yes-or-no-p "Really trash Forge's database file? "))
    (when forge--db-connection
      (emacsql-close forge--db-connection))
    (delete-file forge-database-file t)
    (magit-refresh)))

(defun forge-enable-sql-logging ()
  "Enable logging Forge's SQL queries."
  (interactive)
  (let ((db (forge-db)))
    (emacsql-enable-debugging db)
    (switch-to-buffer-other-window (emacsql-log-buffer db))))

(magit-define-section-jumper forge-jump-to-pullreqs "Pull requests" pullreqs)
(magit-define-section-jumper forge-jump-to-issues "Issues" issues)

;;; _
(provide 'forge-commands)
;;; forge-commands.el ends here
