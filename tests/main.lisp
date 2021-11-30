(defpackage hermes-perception/tests/main
  (:use :cl
        :hermes-perception
        :rove))
(in-package :hermes-perception/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :hermes-perception)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
           (ok (= 1 1))))
