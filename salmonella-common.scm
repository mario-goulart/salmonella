(import scheme)
(cond-expand
 (chicken-4
  (import foreign)
  (use extras files irregex ports posix srfi-1 srfi-13)
  (define pseudo-random-integer random))
 ((or chicken-5 chicken-6)
  (import (chicken file)
          (chicken file posix)
          (chicken foreign)
          (chicken irregex)
          (chicken pathname)
          (chicken port)
          (chicken process-context)
          (chicken random)
          (chicken string))
  (include "libs/srfi-1.scm")
  (include "libs/srfi-13.scm"))
 (else
  (error "Unsupported CHICKEN version.")))

(include "salmonella-version.scm")

;; Used to be chicken-prefix in C4
(define default-installation-prefix (foreign-value "C_INSTALL_PREFIX" c-string))

(define (delete-path . paths)
  ;; We could simply use delete-directory giving it a truthy value as
  ;; second argument to make it recursive, but the recursive mode uses
  ;; find-files, which was broken before
  ;; ba01911d2644dd8ac40eced46a8451033e565d86.  So, we implement a
  ;; simplified version of ##sys#find-files (find) and
  ;; delete-directory (rmdir).

  (define (find dir)
    ;; simplified implementation of ##sys#find-files, which was broken
    ;; before
    (let loop ((files (glob (make-pathname dir "?*")))
               (all-files '()))
      (if (null? files)
          all-files
          (let ((f (car files))
                (rest (cdr files)))
            (if (directory? f)
                (cond ((member (pathname-file f) '("." ".."))
                       (loop rest all-files))
                      ((and (symbolic-link? f))
                       (loop rest (cons f all-files)))
                      (else
                       (loop rest
                             (loop (glob (make-pathname f "?*"))
                                   (cons f all-files)))))
                (loop rest (cons f all-files)))))))

  (define (rmdir name)
    (let ((files (find name)))
      (for-each
       (lambda (f)
         ((if (directory? f)
              delete-directory
              delete-file)
          f))
       files)
      (delete-directory name)))

  (for-each (lambda (path)
              (when (file-exists? path)
                (if (directory? path)
                    (rmdir path)
                    (delete-file path))))
            paths))

(define (get-egg-dependencies meta-data #!key with-test-dependencies?
                                              with-versions?)
  (define (deps key)
    (or (and-let* ((d (assq key meta-data)))
          (cdr d))
        '()))
  (map (lambda (dep)
         (if with-versions?
             dep
             (if (pair? dep)
                 (car dep)
                 dep)))
       (append (deps (cond-expand
                      (chicken-4 'depends)
                      ((or chicken-5 chicken-6) 'dependencies)))
               (deps (cond-expand
                      (chicken-4 'needs)
                      ((or chicken-5 chicken-6) 'build-dependencies)))
               (if with-test-dependencies?
                   (deps (cond-expand
                          (chicken-4 'test-depends)
                          ((or chicken-5 chicken-6) 'test-dependencies)))
                   '()))))

(define (parse-cmd-line cmd-line-args spec)
  ;; spec: list of elements.  The format of elements is:
  ;;  - symbols: options that do not require an argument
  ;;  - lists: options that require an argument.
  ;;  - what doesn't match these patterns is assumed to be a no named arg
  ;; Return a pair (<eggs> . <alist opts>)
  ;; Note: options are supposed to start with `--'.  -h and -help are
  ;;       specially handled.
  (let ((nonamed '())
        (parsed-opts '()))
    (let loop ((args cmd-line-args))
      (unless (null? args)
        (let ((arg (car args)))
          (cond ((string-prefix? "--" arg)
                 (if (substring-index "=" arg)
                     (let* ((parts (string-split arg "="))
                            (param (string->symbol (car parts)))
                            (val (string-intersperse (cdr parts) "=")))
                       (if (memq param spec)
                           (die param " does not take any argument.")
                           (if (alist-ref param (filter pair? spec))
                               (set! parsed-opts (cons (cons param val) parsed-opts))
                               (die "Invalid option: " param))))
                     (let ((param (string->symbol arg)))
                       (if (memq param spec)
                           (set! parsed-opts (cons (cons param #t)  parsed-opts))
                           (if (alist-ref param (filter pair? spec))
                               (die param " requires an argument.")
                               (die "Invalid option: " param))))))
                ((string=? arg "-h")
                 (set! parsed-opts (cons (cons '-h #t) parsed-opts)))
                ((string=? arg "-help")
                 (set! parsed-opts (cons (cons '-help #t) parsed-opts)))
                (else
                 (set! nonamed (cons arg nonamed)))))
        (loop (cdr args))))
    (cons (reverse nonamed) parsed-opts)))

(define (cmd-line-arg option parsed-opts)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS.  ARGS is the cdr of
  ;; the pair returned by parse-cmd-line.
  (let loop ((opts parsed-opts))
    (if (null? opts)
        #f
        (let* ((opt (car opts))
               (param (if (pair? opt)
                          (car opt)
                          opt)))
          (cond ((and (pair? opt) (eq? (car opt) option))
                 (cdr opt))
                ((and (symbol? opt) (eq? opt option))
                 #t)
                (else (loop (cdr opts))))))))

(define (handle-help args usage)
  (when (or (cmd-line-arg '-h args)
            (cmd-line-arg '-help args)
            (cmd-line-arg '--help args))
    (usage)))

(define (handle-version args)
  (when (cmd-line-arg '--version args)
    (print salmonella-version)
    (exit 0)))

(define (die . msg)
  (with-output-to-port (current-error-port)
    (lambda ()
      (for-each display msg)
      (newline)
      (flush-output)))
  (exit 1))


(define (mktempdir)
  ;; For compatibility with older chickens.
  ;; `create-temporary-directory' has been introduced by 4.6.0
  (let loop ()
    (let ((dir (make-pathname
                (current-directory)
                (string-append "salmonella-tmp-"
                               (number->string
                                (pseudo-random-integer 1000000) 16)))))
        (if (file-exists? dir)
            (loop)
            (begin
              (create-directory dir)
              dir)))))

(define (port-for-exit-code exit-code)
  (if (and exit-code (not (zero? exit-code)))
      (current-error-port)
      (current-output-port)))

(define (make-usage show)
  (lambda (exit-code)
    (let ((this (pathname-strip-directory (program-name)))
          (port (port-for-exit-code exit-code)))
      (show this port)
      (when exit-code (exit exit-code)))))

(define (usage #!key exit-code epidemy?)
  (let ((this (pathname-strip-directory (program-name)))
        (port (port-for-exit-code exit-code)))
    (display #<#EOF
#this [ -h | --help ]
#this --version
#this [ [ <options> ] eggs ]

When called without eggs in the command line, salmonella will try to
find a .egg file in the current directory and process it (just like
chicken-install).

<options>:
--log-file=<logfile>
    The name for the log file to be generated by salmonella
    (default=salmonella.log).

--chicken-installation-prefix=<prefix dir>
    If you want to test eggs using a chicken installed on a certain directory,
    you can use this option (it should point to the same directory as given to
    `PREFIX' when installing CHICKEN). If omitted, salmonella uses CHICKEN
    tools from the current runtime's installation prefix.  Note that when this
    option is used, salmonella will look for "csi", "csc" and "chicken-install"
    under <prefix dir>/bin.  If the CHICKEN installed under <prefix dir>
    has different names for the CHICKEN tools (i.e., they were tweaked via
    CSI_PROGRAM, CSC_PROGRAM, CHICKEN_INSTALL_PROGRAM, PROGRAM_PREFIX or
    PROGRAM_SUFFIX at build time), you will need to use the --csi, --csc
    and/or --chicken-install parameters for salmonella.

--chicken-install-args=<install args>
    This option can be used customize chicken-install's arguments.  You can
    use <repo> to indicate where you want the actual repository directory
    to be replaced by salmonella.

--csi=<path to csi>
   Path to csi.  If provided, salmonella will use the csi pointed by
   this parameter instead of the one from the current runtime's
   installation prefix (or the static "csi" from the prefix set by
   --chicken-installation-prefix).

--csc=<path to csc>
   Path to csc.  If provided, salmonella will use the csc pointed by
   this parameter instead of the one from the current runtime's
   installation prefix (or the static "csc" from the prefix set by
   --chicken-installation-prefix).

--chicken-install=<path to chicken-install>
   Path to chicken-install.  If provided, salmonella will use the csi
   pointed by this parameter instead of the one from the current runtime's
   installation prefix (or the static "chicken-install" from the prefix set
   by --chicken-installation-prefix).

--eggs-doc-dir=<doc dir>
    By default, salmonella checks if documentation for eggs exist by accessing
    the CHICKEN wiki.  If you have a local copy of the wiki documentation for
    eggs, you can use this option to point to the directory where they can be
    found.

--keep-repo
    For each egg that salmonella tests, it sets the egg installation repository
    empty and removes it at the end of its execution.  This option makes
    salmonella keep the egg installation repository after testing each egg and
    after finishing its execution.  This option can save a lot of time when
    testing several eggs, at the cost of potentially making salmonella unable
    to catch dependencies problems.

--skip-eggs=<comma-separated list of eggs to skip>
    A comma-separated list of eggs to be skipped.

--repo-dir=<path to repo dir to be used>
    Alternative location for the egg installation directory used by salmonella.
    By default, salmonella generates a `salmonella-tmp-xxxxx' directory in the
    current directory.  This option can be useful when used with `--keep-repo'
    to reuse egg installation repositories for several salmonella executions.

--clear-chicken-home
    Remove Scheme files from <chicken-installation-prefix>/share/chicken.
    WARNING: use this option with care.  If you don't understand the
    consequences of this option, DON'T USE IT.  Extra care when you don't
    use --chicken-installation-prefix -- in this case --clear-chicken-home
    will remove Scheme files from your "host" CHICKEN installation.
    This option is only effective when --keep-repo is NOT used.

--verbosity=<number>
    A number to indicate salmonella's verbosity level.  0 means practically
    silent. 1 is mostly silent and 2 (default) prints some useful information
    while salmonella is running.

EOF
    port)
    (when epidemy?
      (display #<#EOF

--instances=<number>
    Number of salmonella instances to run in parallel.

--salmonella-tools-dir
    Directory where the tools required by salmonella-epidemy (salmonella,
    salmonella-log-merger) can be found.  If omitted, the value of
    C_TARGET_BIN_HOME of the CHICKEN runtime will be used.

EOF
))
    (newline)
    (when exit-code (exit exit-code))))
