;; -*- lexical-binding: t; -*-

;; * ================================================================== *
;; * Tests for pdf-cache.el
;; * ================================================================== *

(require 'pdf-cache)
(require 'ert)

(ert-deftest pdf-cache-get-image ()
  (let (pdf-cache--image-cache)
    (should-not (pdf-cache-get-image 1 1))
    (setq pdf-cache--image-cache
          (list
           (pdf-cache--make-image 1 1 "1" nil)
           (pdf-cache--make-image 2 1 "2" nil)
           (pdf-cache--make-image 3 1 "3" nil)))
    (should (equal (pdf-cache-get-image 1 1) "1"))
    (should (equal pdf-cache--image-cache
                   (list
                    (pdf-cache--make-image 1 1 "1" nil)
                    (pdf-cache--make-image 2 1 "2" nil)
                    (pdf-cache--make-image 3 1 "3" nil))))
    (should (equal (pdf-cache-get-image 2 1) "2"))
    (should (equal pdf-cache--image-cache
                   (list
                    (pdf-cache--make-image 2 1 "2" nil)
                    (pdf-cache--make-image 1 1 "1" nil)
                    (pdf-cache--make-image 3 1 "3" nil))))
    (should-not (pdf-cache-get-image 4 1))))

(ert-deftest pdf-cache-prefetch-start-without-page ()
  "Prefetching skips a window that has no page yet (#310).
A frame made by emacsclient can run the idle timer before its window
has a page."
  (pdf-test-with-test-pdf
    (let ((buffer (current-buffer)))
      (pdf-cache-prefetch-minor-mode 1)
      (cl-letf (((symbol-function 'window-buffer) (lambda (&rest _) buffer))
                ((symbol-function 'image-mode-window-get) #'ignore))
        (should-not (pdf-view-current-page))
        (pdf-cache--prefetch-start buffer)
        (should-not pdf-cache--prefetch-pages)))))
