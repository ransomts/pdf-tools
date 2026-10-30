;; -*- lexical-binding: t -*-

(require 'pdf-loader)
(require 'ert)
(require 'cl-lib)

(ert-deftest pdf-loader-activation ()
  :expected-result :failed ;; Until someone figures out how to run the
                           ;; tests w/o loading all of the package.
  (should-not (memq 'pdf-tools features))
  (pdf-loader-install)
  (with-current-buffer (find-file "test.pdf")
    (should (eq major-mode 'pdf-view-mode))))

(ert-deftest pdf-loader-install/uninstall-alists ()
  (cl-labels ((alists-installed-p ()
                (and (assoc pdf-loader--auto-mode-alist-item
                            auto-mode-alist)
                     (assoc pdf-loader--magic-mode-alist-item
                            magic-mode-alist))))
    ;; Uninstalling removes PDF Tools' own entries too, which share
    ;; the loader's patterns; don't take them from the other tests.
    (let ((auto-mode-alist (copy-sequence auto-mode-alist))
          (magic-mode-alist (copy-sequence magic-mode-alist)))
      (pdf-loader--install #'ignore)
      (should (alists-installed-p))
      (pdf-loader--uninstall)
      (should-not (alists-installed-p)))))
