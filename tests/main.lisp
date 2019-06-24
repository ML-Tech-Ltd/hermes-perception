(defpackage overmind-perception/tests/main
  (:use :cl
        :overmind-perception
        :rove))
(in-package :overmind-perception/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :overmind-perception)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
