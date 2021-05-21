;; (ql:quickload :hermes-perception)
(defpackage hermes-perception
  (:use :cl :alexandria)
  (:import-from #:defenum
		#:defenum)
  (:import-from #:hscom.utils
		#:random-int
		#:assoccess)
  (:import-from #:hsinp.rates
		#:->open
		#:->close
		#:->high
		#:->low)
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
	   #:fixed=>sma-close
	   #:random=>sma-close
	   #:fixed=>sma-close-strategy-1
	   #:random=>sma-close-strategy-1
	   #:fixed=>sma-close-strategy-2
	   #:random=>sma-close-strategy-2
	   #:fixed=>wma-close
	   #:random=>wma-close
	   #:fixed=>ema-close
	   #:random=>ema-close
	   #:fixed=>macd-close
	   #:random=>macd-close
	   #:fixed=>rsi-close
	   #:random=>rsi-close
	   #:fixed=>high-height
	   #:random=>high-height
	   #:fixed=>low-height
	   #:random=>low-height
	   #:fixed=>candle-height
	   #:random=>candle-height
	   #:fixed=>diff-close-frac
	   #:random=>diff-close-frac
	   #:gen-random-perceptions
	   #:gen-perception-fn
	   #:get-perceptions
	   #:nth-perception)
  (:nicknames :hsper))
(in-package :hermes-perception)

(defun =>diff-close (rates offset)
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates))
	 (penultimate-candle (nth (- lrates (1+ offset) 1) rates)))
    (- (hsinp.rates:->close last-candle)
       (hsinp.rates:->close penultimate-candle))))
;; (->diff-close *rates* 0)

(defun =>diff-close-frac (rates offset)
  (hsinp.rates:->close-frac (nth (- (length rates) offset 1) rates)))
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
    (->close (nth (- lrates offset 1) rates))))
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
    (- (->high last-candle)
       (if (> (->open last-candle)
	      (->close last-candle))
	   (->open last-candle)
	   (->close last-candle)))))

(defun =>low-height (rates offset)
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (- (if (> (->open last-candle)
	      (->close last-candle))
	   (->close last-candle)
	   (->open last-candle))
       (->low last-candle))))

(defun =>candle-height (rates offset)
  (let* ((lrates (length rates))
	 (last-candle (nth (- lrates offset 1) rates)))
    (abs (- (->close last-candle)
	    (->open last-candle)))))

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
     =>low-height
     =>candle-height
     ))

(defparameter *docs* (make-hash-table)
  "We're documenting externally to DEFENUM because DEFENUM sadly doesn't support docstrings.")
(let ((offset-doc "Offset relative to current time. 0 represents current time. 1 represents previous time.")
      (n-doc "How many datapoints to consider for the technical indicator.")
      (n-sma "How many datapoints to consider for the SMA.")
      (n-ema "How many datapoints to consider for the EMA.")
      (n-long-sma "How many datapoints to consider for the *long* SMA.")
      (n-long-ema "How many datapoints to consider for the *long* EMA.")
      (n-short-sma "How many datapoints to consider for the *short* SMA.")
      (n-short-ema "How many datapoints to consider for the *short* EMA.")
      (n-signal "How many datapoints to consider for the MACD signal."))
  (setf (gethash '=>sma-close *docs*)
	`((:documentation . "Simple Moving Average (SMA) applied to the =>CLOSE prices.")
	  (:params . (((:name . n) (:default . 10) (:documentation . ,n-doc))
		      ((:name . offset) (:default . 0) (:documentation . ,offset-doc))))
	  (:array-fn . ,#'fixed=>sma-close)))
  (setf (gethash '=>wma-close *docs*)
	`((:documentation . "Weighted Moving Average (WMA) applied to the =>CLOSE prices.")
	  (:params . (((:name . n) (:default . 10) (:documentation . ,n-doc))
		      ((:name . offset) (:default . 0) (:documentation . ,offset-doc))))
	  (:array-fn . ,#'fixed=>wma-close)))
  (setf (gethash '=>ema-close *docs*)
	`((:documentation . "Exponential Moving Average (EMA) applied to the =>CLOSE prices.")
	  (:params . (((:name . n-ema) (:default . 20) (:documentation . ,n-ema))
		      ((:name . n-sma) (:default . 10) (:documentation . ,n-sma))
		      ((:name . offset) (:default . 0) (:documentation . ,offset-doc))))
	  (:array-fn . ,#'fixed=>ema-close)))
  (setf (gethash '=>rsi-close *docs*)
	`((:documentation . "Relative Strength Index (RSI) applied to the =>CLOSE prices.")
	  (:params . (((:name . n) (:default . 10) (:documentation . ,n-doc))
		      ((:name . offset) (:default . 0) (:documentation . ,offset-doc))))
	  (:array-fn . ,#'fixed=>rsi-close)))
  (setf (gethash '=>macd-close *docs*)
	`((:documentation . "Moving Average Convergence Divergence (MACD) applied to the =>CLOSE prices.")
	  (:params . (((:name . n-signal) (:default . 20) (:documentation . ,n-signal))
		      ((:name . n-long-ema) (:default . 20) (:documentation . ,n-long-ema))
		      ((:name . n-long-sma) (:default . 20) (:documentation . ,n-long-sma))
		      ((:name . n-short-ema) (:default . 10) (:documentation . ,n-short-ema))
		      ((:name . n-short-sma) (:default . 10) (:documentation . ,n-short-sma))
		      ((:name . offset) (:default . 0) (:documentation . ,offset-doc))))
	  (:array-fn . ,#'fixed=>macd-close)))
  (setf (gethash '=>diff-close-frac *docs*)
	`((:documentation . "Fractional Difference (fracdiff) applied to the =>CLOSE prices.")
	  (:params . (((:name . offset) (:default . 0) (:documentation . ,offset-doc))))
	  (:array-fn . ,#'fixed=>diff-close-frac)))
  (setf (gethash '=>high-height *docs*)
	`((:documentation . "Price difference between max(=>OPEN, =>CLOSE) and the =>HIGH price of a candle.")
	  (:params . (((:name . offset) (:default . 0) (:documentation . ,offset-doc))))
	  (:array-fn . ,#'fixed=>high-height)))
  (setf (gethash '=>low-height *docs*)
	`((:documentation . "Price difference between min(=>OPEN, =>CLOSE) and the =>LOW price of a candle.")
	  (:params . (((:name . offset) (:default . 0) (:documentation . ,offset-doc))))
	  (:array-fn . ,#'fixed=>low-height)))
  (setf (gethash '=>candle-height *docs*)
	`((:documentation . "Price difference between =>OPEN and =>CLOSE prices of a candle.")
	  (:params . (((:name . offset) (:default . 0) (:documentation . ,offset-doc))))
	  (:array-fn . ,#'fixed=>candle-height)))
  (setf (gethash '=>sma-close-strategy-1 *docs*)
	`((:documentation . "Trading strategy involving one SMA. A bullish signal is represented by the =>CLOSE price crossing an SMA from below. A bearish signal is represented by the =>CLOSE price crossing an SMA from above.")
	  (:params . (((:name . n) (:default . 10) (:documentation . ,n-doc))
		      ((:name . offset) (:default . 0) (:documentation . ,offset-doc))))
	  (:array-fn . ,#'fixed=>sma-close-strategy-1)))
  (setf (gethash '=>sma-close-strategy-2 *docs*)
	`((:documentation . "Trading strategy involving two SMAs. A bullish signal is represented by a fast SMA crossing a slow SMA from below. A bearish signal is represented by a fast SMA crossing a slow SMA from above.")
	  (:params . (((:name . n-long-sma) (:default . 20) (:documentation . ,n-long-sma))
		      ((:name . n-short-sma) (:default . 10) (:documentation . ,n-short-sma))
		      ((:name . offset) (:default . 0) (:documentation . ,offset-doc))))
	  (:array-fn . ,#'fixed=>sma-close-strategy-2))))

(defun get-perceptions ()
  (mapcar (lambda (tag-name)
	    `((:id . ,(eval tag-name))
	      (:name . ,(make-keyword tag-name))
	      (:params . ,(assoccess (gethash tag-name *docs*) :params))
	      (:documentation . ,(assoccess (gethash tag-name *docs*) :documentation))
	      (:array-fn . ,(assoccess (gethash tag-name *docs*) :array-fn))))
	  (defenum:tags (defenum:find-enum 'perceptions))))
;; (get-perceptions)
;; (hscom.utils:assoccess (first (get-perceptions)) :params)

(defun fixed=>sma-close (offset n)
  (values `#(,=>sma-close ,offset ,n)
	  (+ offset n)))
;; (fixed=>sma-close 5 10)

(defun random=>sma-close ()
  (let ((offset (random-int 0 50))
	(n (random-int 3 50)))
    (fixed=>sma-close offset n)))
;; (random=>sma-close)

(defun fixed=>sma-close-strategy-1 (offset n)
  (values `#(,=>sma-close-strategy-1 ,offset ,n)
	  (+ offset n 1)))

(defun random=>sma-close-strategy-1 ()
  (let ((offset (random-int 0 50))
	(n (random-int 3 50)))
    (fixed=>sma-close-strategy-1 offset n)))

(defun fixed=>sma-close-strategy-2 (offset n-short-sma n-long-sma)
  (values `#(,=>sma-close-strategy-2 ,offset ,n-short-sma ,n-long-sma)
	  (+ offset n-short-sma n-long-sma 1)))

(defun random=>sma-close-strategy-2 ()
  (let ((offset (random-int 0 50))
	(n-short-sma (random-int 3 50))
	(n-long-sma (random-int 3 50)))
    (fixed=>sma-close-strategy-2 offset n-short-sma n-long-sma)))

(defun fixed=>wma-close (offset n)
  (values `#(,=>wma-close ,offset ,n)
	  (+ offset n)))

(defun random=>wma-close ()
  (let ((offset (random-int 0 50))
	(n (random-int 3 50)))
    (fixed=>wma-close offset n)))

(defun fixed=>ema-close (offset n-sma n-ema)
  (values `#(,=>ema-close ,offset ,n-sma ,n-ema)
	  (+ offset n-sma n-ema)))

(defun random=>ema-close ()
  (let ((offset (random-int 0 50))
	(n-sma (random-int 3 25))
	(n-ema (random-int 3 25)))
    (fixed=>ema-close offset n-sma n-ema)))

(defun fixed=>macd-close (offset n-short-sma n-short-ema n-long-sma n-long-ema n-signal)
  (values `#(,=>macd-close ,offset ,n-short-sma ,n-short-ema ,n-long-sma ,n-long-ema ,n-signal)
	  (+ offset (* 2 (max n-short-sma n-short-ema n-long-sma n-long-ema)) n-signal)))

(defun random=>macd-close ()
  (let ((offset (random-int 0 50))
	(n-short-sma (random-int 3 25))
	(n-short-ema (random-int 3 25))
	(n-long-sma (random-int 3 25))
	(n-long-ema (random-int 3 25))
	(n-signal (random-int 3 25)))
    (fixed=>macd-close offset n-short-sma n-short-ema n-long-sma n-long-ema n-signal)))

(defun fixed=>rsi-close (offset n)
  (values `#(,=>rsi-close ,offset ,n)
	  (+ offset n)))

(defun random=>rsi-close ()
  (let ((offset (random-int 0 50))
	(n (random-int 10 20)))
    (fixed=>rsi-close offset n)))

(defun fixed=>high-height (offset)
  (values `#(,=>high-height ,offset)
	  offset))

(defun random=>high-height ()
  (let ((offset (random-int 0 50)))
    (fixed=>high-height offset)))

(defun fixed=>low-height (offset)
  (values `#(,=>low-height ,offset)
	  offset))

(defun random=>low-height ()
  (let ((offset (random-int 0 50)))
    (fixed=>low-height offset)))

(defun fixed=>candle-height (offset)
  (values `#(,=>candle-height ,offset)
	  offset))

(defun random=>candle-height ()
  (let ((offset (random-int 0 50)))
    (fixed=>candle-height offset)))

(defun fixed=>diff-close-frac (offset)
  (values `#(,=>diff-close-frac ,offset)
	  offset))

(defun random=>diff-close-frac ()
  (let ((offset (random-int 0 50)))
    (fixed=>diff-close-frac offset)))

(defun gen-random-perceptions (fns-count)
  (let ((fns-bag `(,#'random=>sma-close
		   ,#'random=>sma-close-strategy-1
		   ,#'random=>sma-close-strategy-2
		   ,#'random=>wma-close
		   ,#'random=>ema-close
		   ,#'random=>rsi-close
		   ;; ,#'random=>macd-close
		   ;; ,#'random=>high-height
		   ;; ,#'random=>low-height
		   ;; ,#'random=>candle-height
		   ;; ,#'random=>diff-close-frac
		   ))
	(max-lookbehind 0)
	(perceptions))
    (loop repeat fns-count
	  do (multiple-value-bind (perc lookbehind)
		      (funcall (random-elt fns-bag))
		    (when (> lookbehind max-lookbehind)
		      (setf max-lookbehind lookbehind))
		    (push perc perceptions)))
    `((:perception-fns . ,(make-array (length perceptions) :initial-contents perceptions))
      (:lookahead-count . ,(if hscom.hsage:*random-lookahead-p*
			       (random-int
				hscom.hsage:*random-lookahead-min*
				hscom.hsage:*random-lookahead-max*)
			       hscom.hsage:*lookahead*))
      (:lookbehind-count . ,(+ 10 max-lookbehind)))))
;; (gen-random-perceptions 10)

(defun gen-perception-fn (perception-fns)
  (lambda (rates)
    (loop for fn across perception-fns
	  collect (apply #'funcall
			 (defenum:nth-enum-tag (aref fn 0) 'perceptions)
			 rates (coerce (subseq fn 1) 'list)))))
;; (time (loop repeat 1000 do (funcall (gen-perception-fn #(#(0 0 10) #(0 1 10) #(1 0))) *rates*)))

(defun nth-perception (tag)
  (defenum:nth-enum-tag tag 'perceptions))
;; (nth-perception 0)
