;; (ql:quickload :hermes-perception)
(defpackage hermes-perception
  (:use :cl :alexandria)
  (:import-from #:defenum
		#:defenum)
  (:import-from #:omcom.utils
		#:random-int)
  (:export #:=>diff-close
	   #:=>diff-close-frac
	   #:=>sma-close
	   #:=>sma-close-strategy-1
	   #:=>sma-close-strategy-2
	   #:=>wma-close
	   #:=>close
	   #:=>ema-close
	   #:=>macd-close
	   #:=>rsi-close
	   #:=>high-height
	   #:=>low-height
	   #:=>candle-height
	   #:gen-random-perceptions
	   #:gen-perception-fn
	   )
  (:nicknames :omper))
(in-package :hermes-perception)

(defun =>diff-close (rates offset)
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates))
	 (penultimate-candle (nth (- lrates (1+ offset) 1) rates)))
    (- (ominp.rates:->close last-candle)
       (ominp.rates:->close penultimate-candle))))
;; (->diff-close *rates* 0)

(defun =>diff-close-frac (rates offset)
  (ominp.rates:->close-frac (nth (- (length rates) offset 1) rates)))
;; (=>diff-close-frac *rates* 10)

(defun =>sma-close (rates offset n)
  (/ (loop for i below n
	   summing (=>diff-close-frac rates (+ i offset)))
     n))

(defun =>sma-close-strategy-1 (rates offset n)
  (let ((close-0 (=>diff-close-frac rates offset))
	(close-1 (=>diff-close-frac rates (1+ offset)))
	(sma (=>sma-close rates offset n)))
    (if (or (and (< close-0 sma)
		 (> close-1 sma))
	    (and (> close-0 sma)
		 (< close-1 sma)))
	(- sma close-0)
	0)))
(defun =>sma-close-strategy-2 (rates offset n-short-sma n-long-sma)
  (let ((short-sma-0 (=>sma-close rates offset (min n-short-sma n-long-sma)))
	(short-sma-1 (=>sma-close rates (1+ offset) (min n-short-sma n-long-sma)))
	(long-sma (=>sma-close rates offset (max n-short-sma n-long-sma))))
    (if (or (and (< short-sma-0 long-sma)
		 (> short-sma-1 long-sma))
	    (and (> short-sma-0 long-sma)
		 (< short-sma-1 long-sma)))
	(- short-sma-0 long-sma)
	0)))
(defun =>wma-close (rates offset n)
  (/ (loop
       for i below n
       for j from n above 0
       summing (* j (=>diff-close-frac rates (+ i offset))))
     (* n (1+ n) 1/2)))

(defun =>close (rates offset)
  (let* ((lrates (length rates)))
    (rate-close (nth (- lrates offset 1) rates))))
;; (->close *rates* 0)

(defun =>ema-close (rates offset n-sma n-ema)
  (let ((smoothing (/ 2 (1+ n-ema)))
	(ema (=>sma-close rates (+ offset n-ema) n-sma)))
    (loop for i from (1- n-ema) downto 1
	  do (setf ema (+ ema (* smoothing (- (=>diff-close-frac rates (+ i offset)) ema)))))
    ema))

(defun =>macd-close (rates offset n-short-sma n-short-ema n-long-sma n-long-ema n-signal)
  (let ((last-macd 0)
	(signal 0))
    (loop for off from (1- n-signal) downto 0
	  do (let* ((short-ema (=>ema-close rates (+ off offset) n-short-sma n-short-ema))
		    (long-ema (=>ema-close rates (+ off offset) n-long-sma n-long-ema))
		    (macd (- short-ema long-ema)))
	       (incf signal macd)
	       (setf last-macd macd)))
    (- last-macd (/ signal n-signal))))

(defun =>rsi-close (rates offset n)
  (let ((gain 0)
	(loss 0))
    (loop for i from 0 below n
	  do (let ((delta (=>diff-close-frac rates (+ offset i))))
	       (if (plusp delta)
		   (incf gain delta)
		   (incf loss (abs delta)))))
    (if (= loss 0)
	100
	(let ((rs (/ (/ gain n) (/ loss n))))
	  (- 100 (/ 100 (1+ rs)))))))

(defun =>high-height (rates offset)
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (- (rate-high last-candle)
       (if (> (rate-open last-candle)
	      (rate-close last-candle))
	   (rate-open last-candle)
	   (rate-close last-candle)))))

(defun =>low-height (rates offset)
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (- (if (> (rate-open last-candle)
	      (rate-close last-candle))
	   (rate-close last-candle)
	   (rate-open last-candle))
       (rate-low last-candle))))

(defun =>candle-height (rates offset)
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (abs (- (rate-close last-candle)
	    (rate-open last-candle)))))

(defenum perceptions
    (=>sma-close
     =>sma-close-strategy-1
     =>sma-close-strategy-2
     =>wma-close
     =>ema-close
     =>rsi-close

     ;; Unused list:
     =>macd-close
     =>diff-close-frac
     =>high-height
     ->low-height
     ->candle-height
     ))

(defun random=>sma-close ()
  (let ((offset (random-int 0 50))
	(n (random-int 3 50)))
    (values `#(,=>sma-close ,offset ,n)
	    (+ offset n))))
(defun random=>sma-close-strategy-1 ()
  (let ((offset (random-int 0 50))
	(n (random-int 3 50)))
    (values `#(,=>sma-close-strategy-1 ,offset ,n)
	    (+ offset n 1))))
(defun random=>sma-close-strategy-2 ()
  (let ((offset (random-int 0 50))
	(n-short-sma (random-int 3 50))
	(n-long-sma (random-int 3 50)))
    (values `#(,=>sma-close-strategy-2 ,offset ,n-short-sma ,n-long-sma)
	    (+ offset n-short-sma n-long-sma 1))))
(defun random=>wma-close ()
  (let ((offset (random-int 0 50))
	(n (random-int 3 50)))
    (values `#(,=>wma-close ,offset ,n)
	    (+ offset n))))
(defun random=>ema-close ()
  (let ((offset (random-int 0 50))
	(n-sma (random-int 3 25))
	(n-ema (random-int 3 25)))
    (values `#(,=>ema-close ,offset ,n-sma ,n-ema)
	    (+ offset n-sma n-ema))))
(defun random=>macd-close ()
  (let ((offset (random-int 0 50))
	(n-short-sma (random-int 3 25))
	(n-short-ema (random-int 3 25))
	(n-long-sma (random-int 3 25))
	(n-long-ema (random-int 3 25))
	(n-signal (random-int 3 25)))
    (values `#(,=>macd-close ,offset ,n-short-sma ,n-short-ema ,n-long-sma ,n-long-ema ,n-signal)
	    (+ offset (* 2 (max n-short-sma n-short-ema n-long-sma n-long-ema)) n-signal))))
(defun random=>rsi-close ()
  (let ((offset (random-int 0 50))
	(n (random-int 10 20)))
    (values `#(,=>rsi-close ,offset ,n)
	    (+ offset n))))
(defun random=>high-height ()
  (let ((offset (random-int 0 50)))
    (values `#(,=>high-height ,offset)
	    offset)))
(defun random->low-height ()
  (let ((offset (random-int 0 50)))
    (values `#(,->low-height ,offset)
	    offset)))
(defun random->candle-height ()
  (let ((offset (random-int 0 50)))
    (values `#(,->candle-height ,offset)
	    offset)))
(defun random=>diff-close-frac ()
  (let ((offset (random-int 0 50)))
    (values `#(,=>diff-close-frac ,offset)
	    offset)))

(defun gen-random-perceptions (fns-count)
  (let ((fns-bag `(,#'random=>sma-close
		   ,#'random=>sma-close-strategy-1
		   ,#'random=>sma-close-strategy-2
		   ,#'random=>wma-close
		   ,#'random=>ema-close
		   ,#'random=>rsi-close
		   ;; ,#'random=>macd-close
		   ;; ,#'random=>high-height
		   ;; ,#'random->low-height
		   ;; ,#'random->candle-height
		   ;; ,#'random->diff-close-frac
		   ))
	(max-lookbehind 0)
	(perceptions))
    (loop repeat fns-count
	  collect (multiple-value-bind (perc lookbehind)
		      (funcall (random-elt fns-bag))
		    (when (> lookbehind max-lookbehind)
		      (setf max-lookbehind lookbehind))
		    (push perc perceptions)))
    `((:perception-fns . ,(make-array (length perceptions) :initial-contents perceptions))
      (:lookahead-count . ,(if omcom.omage:*random-lookahead-p*
			       (random-int
				omcom.omage:*random-lookahead-min*
				omcom.omage:*random-lookahead-max*)
			       omcom.omage:*lookahead*))
      (:lookbehind-count . ,(+ 10 max-lookbehind)))))
;; (gen-random-perceptions 30)

(defun gen-perception-fn (perception-fns)
  (lambda (rates)
    (loop for fn across perception-fns
	  collect (apply #'funcall
			 (defenum:nth-enum-tag (aref fn 0) 'perceptions)
			 rates (coerce (subseq fn 1) 'list)))))
;; (time (loop repeat 1000 do (funcall (gen-perception-fn #(#(0 0 10) #(0 1 10) #(1 0))) *rates*)))
